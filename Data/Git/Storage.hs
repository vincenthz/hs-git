{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Data.Git.Storage
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unix
--

module Data.Git.Storage
    ( Git
    , packedNamed
    , gitRepoPath
    , configs
    -- * opening repositories
    , openRepo
    , closeRepo
    , withRepo
    , withCurrentRepo
    , findRepoMaybe
    , findRepo
    , isRepo
    -- * creating repositories
    , initRepo
    -- * repository accessors
    , getDescription
    , setDescription
    -- * iterators
    , iterateIndexes
    , findReference
    , findReferencesWithPrefix
    -- * getting objects
    , getObjectRaw
    , getObjectRawAt
    , getObject
    , getObject_
    , getObjectAt
    , getObjectType
    -- * setting objects
    , setObject
    ) where

import Control.Exception
import qualified Control.Exception as E
import Control.Monad

import Data.List ((\\), isPrefixOf)
import Data.Either (partitionEithers)
import Data.IORef
import Data.Word
import Data.Typeable

import Data.Git.Named
import Data.Git.Imports
import Data.Git.OS
import Data.Git.Path (packedRefsPath)
import Data.Git.Delta
import Data.Git.Storage.FileReader
import Data.Git.Storage.PackIndex
import Data.Git.Storage.Object
import Data.Git.Storage.Pack
import Data.Git.Storage.Loose
import Data.Git.Storage.CacheFile
import Data.Git.Ref
import Data.Git.Config
import qualified Data.ByteString.Lazy as L

import qualified Data.Map as M

data PackIndexReader = PackIndexReader PackIndexHeader FileReader

-- | this is a cache representation of the packed-ref file
type CachedPackedRef hash = CacheFile (PackedRefs (M.Map RefName (Ref hash)))

-- | represent a git repo, with possibly already opened filereaders
-- for indexes and packs
data Git hash = Git
    { gitRepoPath  :: LocalPath
    , indexReaders :: IORef [(Ref hash, PackIndexReader)]
    , packReaders  :: IORef [(Ref hash, FileReader)]
    , packedNamed  :: CachedPackedRef hash
    , configs      :: IORef [Config]
    }

-- | open a new git repository context
openRepo :: LocalPath -> IO (Git SHA1)
openRepo path = Git path <$> newIORef []
                         <*> newIORef []
                         <*> packedRef
                         <*> (readConfigs >>= newIORef)
  where packedRef = newCacheVal (packedRefsPath path)
                                (readPackedRefs path M.fromList)
                                (PackedRefs M.empty M.empty M.empty)
        readConfigs = do
            global <- E.try readGlobalConfig :: IO (Either IOException Config)
            local  <- E.try (readConfig path)
            return $ snd $ partitionEithers [local,global]

-- | close a git repository context, closing all remaining fileReaders.
closeRepo :: Git hash -> IO ()
closeRepo (Git { indexReaders = ireaders, packReaders = preaders }) = do
    mapM_ (closeIndexReader . snd) =<< readIORef ireaders
    mapM_ (fileReaderClose . snd) =<< readIORef preaders
  where closeIndexReader (PackIndexReader _ fr) = fileReaderClose fr

-- | Find the git repository from the current directory.
--
-- If the environment variable GIT_DIR is set then it's used,
-- otherwise iterate from current directory, up to 128 parents for a .git directory
findRepoMaybe :: IO (Maybe LocalPath)
findRepoMaybe = do
    menvDir <- E.catch (Just <$> getEnvAsPath "GIT_DIR") (\(_:: SomeException) -> return Nothing)
    case menvDir of
        Nothing     -> getWorkingDirectory >>= \pwd -> checkDir 0 (pwd </> ".git")
        Just envDir -> isRepo envDir >>= \e -> return (if e then Just envDir else Nothing)
  where checkDir :: Int -> LocalPath -> IO (Maybe LocalPath)
        checkDir 128 _  = return Nothing
        checkDir n   fp = do
            let filepath = fp </> ".git"
            e <- isRepo filepath
            if e then return (Just fp) else checkDir (n+1) (if absolute fp then parent fp else fp </> "..")

-- | Find the git repository from the current directory.
--
-- If the environment variable GIT_DIR is set then it's used,
-- otherwise iterate from current directory, up to 128 parents for a .git directory
findRepo :: IO LocalPath
findRepo = do
    menvDir <- E.catch (Just <$> getEnvAsPath "GIT_DIR") (\(_:: SomeException) -> return Nothing)
    case menvDir of
        Nothing     -> getWorkingDirectory >>= \pwd -> checkDir 0 (pwd </> ".git")
        Just envDir -> do
            e <- isRepo envDir
            when (not e) $ error "environment GIT_DIR is not a git repository"
            return envDir
  where checkDir :: Int -> LocalPath -> IO LocalPath
        checkDir 128 _  = error "not a git repository"
        checkDir n   fp = do
            e <- isRepo fp
            if e then return fp else checkDir (n+1) (if absolute fp then parent fp else fp </> "..")

-- | execute a function f with a git context.
withRepo :: LocalPath -> (Git SHA1 -> IO c) -> IO c
withRepo path f = bracket (openRepo path) closeRepo f

-- | execute a function on the current repository.
--
-- check findRepo to see how the git repository is found.
withCurrentRepo :: (Git SHA1 -> IO a) -> IO a
withCurrentRepo f = findRepo >>= \path -> withRepo path f

-- | basic checks to see if a specific path looks like a git repo.
isRepo :: LocalPath -> IO Bool
isRepo path = do
    dir     <- isDirectory path
    subDirs <- mapM (isDirectory . (path </>))
                    [ "hooks", "info"
                    , "objects", "refs"
                    , "refs"</> "heads", "refs"</> "tags"]
    return $ and ([dir] ++ subDirs)

-- | initialize a new repository at a specific location.
initRepo :: LocalPath -> IO ()
initRepo path = do
    exists <- isDirectory path
    when exists $ error "destination directory already exists"
    createParentDirectory path
    createDirectory False path
    mapM_ (createDirectory False . (path </>))
        [ "branches", "hooks", "info"
        , "logs", "objects", "refs"
        , "refs"</> "heads", "refs"</> "tags"]

-- | read the repository's description
getDescription :: Git hash -> IO (Maybe String)
getDescription git = do
    isdescription <- isFile descriptionPath
    if (isdescription)
        then do
                content <- readTextFile descriptionPath
                return $ Just content
        else return Nothing
  where descriptionPath = (gitRepoPath git) </> "description"

-- | set the repository's description
setDescription :: Git hash -> String -> IO ()
setDescription git desc = do
    writeTextFile descriptionPath desc
  where descriptionPath = (gitRepoPath git) </> "description"

iterateIndexes :: HashAlgorithm hash
               => Git hash
               -> (b -> (Ref hash, PackIndexReader) -> IO (b, Bool))
               -> b -> IO b
iterateIndexes git f initAcc = do
    allIndexes    <- packIndexEnumerate (gitRepoPath git)
    readers       <- readIORef (indexReaders git)
    (a,terminate) <- loop initAcc readers
    if terminate
        then return a
        else readRemainingIndexes a (allIndexes \\ map fst readers)
  where loop acc []     = return (acc, False)
        loop acc (r:rs) = do
            (nacc, terminate) <- f acc r
            if terminate
                then return (nacc,True)
                else loop nacc rs

        readRemainingIndexes acc []            = return acc
        readRemainingIndexes acc (idxref:idxs) = do
            fr <- packIndexOpen (gitRepoPath git) idxref
            idx <- packIndexReadHeader fr
            let idxreader = PackIndexReader idx fr
            let r = (idxref, idxreader)
            modifyIORef (indexReaders git) (\l -> r : l)
            (nacc, terminate) <- f acc r
            if terminate
                then return nacc
                else readRemainingIndexes nacc idxs

-- | Get the object location of a specific reference
findReference :: HashAlgorithm hash => Git hash -> Ref hash -> IO (ObjectLocation hash)
findReference git ref = maybe NotFound id <$> (findLoose `mplusIO` findInIndexes)
  where --findLoose :: HashAlgorithm hash => IO (Maybe (ObjectLocation hash))
        findLoose = do
            isLoose <- looseExists (gitRepoPath git) ref
            if isLoose then return (Just $ Loose ref) else return Nothing

        --findInIndexes :: HashAlgorithm hash => IO (Maybe (ObjectLocation hash))
        findInIndexes = iterateIndexes git isinIndex Nothing --f -> (a -> IndexReader -> IO (a,Bool)) -> a -> IO a

        isinIndex acc (idxref, (PackIndexReader idxhdr indexreader)) = do
            mloc <- packIndexGetReferenceLocation idxhdr indexreader ref
            case mloc of
                Nothing  -> return (acc, False)
                Just loc -> return (Just $ Packed idxref loc, True)

        mplusIO :: IO (Maybe a) -> IO (Maybe a) -> IO (Maybe a)
        mplusIO f g = f >>= \vopt -> case vopt of
            Nothing -> g
            Just v  -> return $ Just v

-- | get all the references that start by a specific prefix
findReferencesWithPrefix :: HashAlgorithm hash => Git hash -> String -> IO [Ref hash]
findReferencesWithPrefix git pre
    | invalidLength         = error ("not a valid prefix: " ++ show pre)
    | not (isHexString pre) = error ("reference prefix contains non hexchar: " ++ show pre)
    | otherwise             = do
        looseRefs  <- looseEnumerateWithPrefixFilter (gitRepoPath git) (take 2 pre) matchRef
        packedRefs <- concat <$> iterateIndexes git idxPrefixMatch []
        return (looseRefs ++ packedRefs)
  where -- not very efficient way to do that... will do for now.
        matchRef ref = pre `isPrefixOf` toHexString ref
        invalidLength = length pre < 2 || length pre > 39

        idxPrefixMatch acc (_, (PackIndexReader idxhdr indexreader)) = do
            refs <- packIndexGetReferencesWithPrefix idxhdr indexreader pre
            return (refs:acc,False)

readRawFromPack :: HashAlgorithm hash => Git hash -> Ref hash -> Word64 -> IO (FileReader, (PackedObjectRaw hash))
readRawFromPack git pref offset = do
    readers <- readIORef (packReaders git)
    reader  <- maybe getDefault return $ lookup pref readers
    po <- packReadRawAtOffset reader offset
    return (reader, po)
  where getDefault = do p <- packOpen (gitRepoPath git) pref
                        modifyIORef (packReaders git) ((pref, p):)
                        return p

readFromPack :: HashAlgorithm hash => Git hash -> Ref hash -> Word64 -> Bool -> IO (Maybe (ObjectInfo hash))
readFromPack git pref o resolveDelta = do
    (reader, x) <- readRawFromPack git pref o
    if resolveDelta then resolve reader o x else return $ Just $ generifyHeader x
  where generifyHeader :: PackedObjectRaw hash -> ObjectInfo hash
        generifyHeader (po, objData) = ObjectInfo { oiHeader = hdr, oiData = objData, oiChains = [] }
          where hdr = (poiType po, poiActualSize po, poiExtra po)

        --resolve :: FileReader -> Word64 -> PackedObjectRaw hash -> IO (Maybe (ObjectInfo hash))
        resolve reader offset (po, objData) = do
            case (poiType po, poiExtra po) of
                (TypeDeltaOff, Just ptr@(PtrOfs doff)) -> do
                    let delta = deltaRead (L.toChunks objData)
                    let noffset = offset - doff
                    base <- resolve reader noffset =<< packReadRawAtOffset reader noffset
                    return $ addToChain ptr $ applyDelta delta base
                (TypeDeltaRef, Just ptr@(PtrRef bref)) -> do
                    let delta = deltaRead (L.toChunks objData)
                    base <- getObjectRaw git bref True
                    return $ addToChain ptr $ applyDelta delta base
                _ ->
                    return $ Just $ generifyHeader (po, objData)

        addToChain ptr (Just oi) = Just (oi { oiChains = ptr : oiChains oi })
        addToChain _   Nothing   = Nothing

        applyDelta :: Maybe Delta -> Maybe (ObjectInfo hash) -> Maybe (ObjectInfo hash)
        applyDelta (Just delta@(Delta _ rSize _)) (Just objInfo) = Just $ objInfo
            { oiHeader = (\(a,_,c) -> (a,rSize,c)) $ oiHeader objInfo
            , oiData   = deltaApply (oiData objInfo) delta
            }
        applyDelta _ _                                      = Nothing

-- | get an object from repository
getObjectRawAt :: HashAlgorithm hash => Git hash -> ObjectLocation hash -> Bool -> IO (Maybe (ObjectInfo hash))
getObjectRawAt _   NotFound    _ = return Nothing
getObjectRawAt git (Loose ref) _ = Just . (\(h,d)-> ObjectInfo h d[]) <$> looseReadRaw (gitRepoPath git) ref
getObjectRawAt git (Packed pref o) resolveDelta = readFromPack git pref o resolveDelta

-- | get an object from repository
getObjectRaw :: HashAlgorithm hash => Git hash -> Ref hash -> Bool -> IO (Maybe (ObjectInfo hash))
getObjectRaw git ref resolveDelta = do
    loc <- findReference git ref
    getObjectRawAt git loc resolveDelta

-- | get an object type from repository
getObjectType :: HashAlgorithm hash => Git hash -> Ref hash -> IO (Maybe ObjectType)
getObjectType git ref = findReference git ref >>= getObjectTypeAt
  where getObjectTypeAt NotFound        = return Nothing
        getObjectTypeAt (Loose _)       = Just . (\(t,_,_) -> t) <$> looseReadHeader (gitRepoPath git) ref
        getObjectTypeAt (Packed pref o) =
            fmap ((\(ty,_,_) -> ty) . oiHeader) <$> readFromPack git pref o True

-- | get an object from repository using a location to reference it.
getObjectAt :: HashAlgorithm hash => Git hash -> ObjectLocation hash -> Bool -> IO (Maybe (Object hash))
getObjectAt git loc resolveDelta = maybe Nothing toObj <$> getObjectRawAt git loc resolveDelta
  where toObj (ObjectInfo { oiHeader = (ty, _, extra), oiData = objData }) = packObjectFromRaw (ty, extra, objData)

-- | get an object from repository using a ref.
getObject :: HashAlgorithm hash
          => Git hash          -- ^ repository
          -> Ref hash          -- ^ the object's reference to
          -> Bool              -- ^ whether to resolve deltas if found
          -> IO (Maybe (Object hash)) -- ^ returned object if found
getObject git ref resolveDelta = maybe Nothing toObj <$> getObjectRaw git ref resolveDelta
  where toObj (ObjectInfo { oiHeader = (ty, _, extra), oiData = objData }) = packObjectFromRaw (ty, extra, objData)

-- | Just like 'getObject' but will raise a RefNotFound exception if the
-- reference cannot be found.
getObject_ :: (Typeable hash, HashAlgorithm hash)
           => Git hash  -- ^ repository
           -> Ref hash  -- ^ the object's reference to
           -> Bool      -- ^ whether to resolve deltas if found
           -> IO (Object hash) -- ^ returned object if found
getObject_ git ref resolveDelta = maybe (throwIO $ RefNotFound ref) return
                              =<< getObject git ref resolveDelta

-- | set an object in the store and returns the new ref
-- this is always going to create a loose object.
setObject :: HashAlgorithm hash
          => Git hash
          -> Object hash
          -> IO (Ref hash)
setObject git obj = looseWrite (gitRepoPath git) obj
