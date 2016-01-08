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

import Filesystem
import Filesystem.Path hiding (concat)
import Filesystem.Path.Rules
import System.Environment

import Control.Applicative
import Control.Exception
import qualified Control.Exception as E
import Control.Monad

import Data.String
import Data.List ((\\), isPrefixOf)
import Data.Either (partitionEithers)
import Data.IORef
import Data.Word

import Data.Git.Named
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

import qualified Data.Map as M

import Prelude hiding (FilePath)

data PackIndexReader = PackIndexReader PackIndexHeader FileReader

-- | this is a cache representation of the packed-ref file
type CachedPackedRef = CacheFile (PackedRefs (M.Map RefName Ref))

-- | represent a git repo, with possibly already opened filereaders
-- for indexes and packs
data Git = Git
    { gitRepoPath  :: FilePath
    , indexReaders :: IORef [(Ref, PackIndexReader)]
    , packReaders  :: IORef [(Ref, FileReader)]
    , packedNamed  :: CachedPackedRef
    , configs      :: IORef [Config]
    }

-- | open a new git repository context
openRepo :: FilePath -> IO Git
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
closeRepo :: Git -> IO ()
closeRepo (Git { indexReaders = ireaders, packReaders = preaders }) = do
    mapM_ (closeIndexReader . snd) =<< readIORef ireaders
    mapM_ (fileReaderClose . snd) =<< readIORef preaders
  where closeIndexReader (PackIndexReader _ fr) = fileReaderClose fr

-- | Find the git repository from the current directory.
--
-- If the environment variable GIT_DIR is set then it's used,
-- otherwise iterate from current directory, up to 128 parents for a .git directory
findRepoMaybe :: IO (Maybe FilePath)
findRepoMaybe = do
    menvDir <- E.catch (Just . decodeString posix_ghc704 <$> getEnv "GIT_DIR") (\(_:: SomeException) -> return Nothing)
    case menvDir of
        Nothing     -> getWorkingDirectory >>= checkDir 0
        Just envDir -> isRepo envDir >>= \e -> return (if e then Just envDir else Nothing)
  where checkDir :: Int -> FilePath -> IO (Maybe FilePath)
        checkDir 128 _  = return Nothing
        checkDir n   wd = do
            let filepath = wd </> ".git"
            e <- isRepo filepath
            if e then return (Just filepath) else checkDir (n+1) (if absolute wd then parent wd else wd </> "..")

-- | Find the git repository from the current directory.
--
-- If the environment variable GIT_DIR is set then it's used,
-- otherwise iterate from current directory, up to 128 parents for a .git directory
findRepo :: IO FilePath
findRepo = do
    menvDir <- E.catch (Just . decodeString posix_ghc704 <$> getEnv "GIT_DIR") (\(_:: SomeException) -> return Nothing)
    case menvDir of
        Nothing     -> getWorkingDirectory >>= checkDir 0
        Just envDir -> do
            e <- isRepo envDir
            when (not e) $ error "environment GIT_DIR is not a git repository" 
            return envDir
  where checkDir :: Int -> FilePath -> IO FilePath
        checkDir 128 _  = error "not a git repository"
        checkDir n   wd = do
            let filepath = wd </> ".git"
            e <- isRepo filepath
            if e then return filepath else checkDir (n+1) (if absolute wd then parent wd else wd </> "..")

-- | execute a function f with a git context.
withRepo path f = bracket (openRepo path) closeRepo f

-- | execute a function on the current repository.
--
-- check findRepo to see how the git repository is found.
withCurrentRepo :: (Git -> IO a) -> IO a
withCurrentRepo f = findRepo >>= \path -> withRepo path f

-- | basic checks to see if a specific path looks like a git repo.
isRepo :: FilePath -> IO Bool
isRepo path = do
    dir     <- isDirectory path
    subDirs <- mapM (isDirectory . (path </>))
                    [ "hooks", "info"
                    , "objects", "refs"
                    , "refs"</> "heads", "refs"</> "tags"]
    return $ and ([dir] ++ subDirs)

-- | initialize a new repository at a specific location.
initRepo :: FilePath -> IO ()
initRepo path = do
    exists <- isDirectory path
    when exists $ error "destination directory already exists"
    createDirectory True path
    mapM_ (createDirectory False . (path </>))
        [ "branches", "hooks", "info"
        , "logs", "objects", "refs"
        , "refs"</> "heads", "refs"</> "tags"]

-- | read the repository's description
getDescription :: Git -> IO (Maybe String)
getDescription git = do
    isdescription <- isFile descriptionPath
    if (isdescription)
        then do
                content <- Prelude.readFile $ encodeString posix descriptionPath
                return $ Just content
        else return Nothing
  where descriptionPath = (gitRepoPath git) </> "description"

-- | set the repository's description
setDescription :: Git -> String -> IO ()
setDescription git desc = do
    Prelude.writeFile (encodeString posix descriptionPath) desc
  where descriptionPath = (gitRepoPath git) </> "description"

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
findReference :: Git -> Ref -> IO ObjectLocation
findReference git ref = maybe NotFound id <$> (findLoose `mplusIO` findInIndexes)
  where findLoose :: IO (Maybe ObjectLocation)
        findLoose = do
            isLoose <- looseExists (gitRepoPath git) ref
            if isLoose then return (Just $ Loose ref) else return Nothing

        findInIndexes :: IO (Maybe ObjectLocation)
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
findReferencesWithPrefix :: Git -> String -> IO [Ref]
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

readRawFromPack :: Git -> Ref -> Word64 -> IO (FileReader, PackedObjectRaw)
readRawFromPack git pref offset = do
    readers <- readIORef (packReaders git)
    reader  <- maybe getDefault return $ lookup pref readers
    po <- packReadRawAtOffset reader offset
    return (reader, po)
  where getDefault = do p <- packOpen (gitRepoPath git) pref
                        modifyIORef (packReaders git) ((pref, p):)
                        return p

readFromPack :: Git -> Ref -> Word64 -> Bool -> IO (Maybe ObjectInfo)
readFromPack git pref o resolveDelta = do
    (reader, x) <- readRawFromPack git pref o
    if resolveDelta then resolve reader o x else return $ Just $ generifyHeader x
  where generifyHeader :: PackedObjectRaw -> ObjectInfo
        generifyHeader (po, objData) = ObjectInfo { oiHeader = hdr, oiData = objData, oiChains = [] }
          where hdr = (poiType po, poiActualSize po, poiExtra po)

        resolve :: FileReader -> Word64 -> PackedObjectRaw -> IO (Maybe ObjectInfo)
        resolve reader offset (po, objData) = do
            case (poiType po, poiExtra po) of
                (TypeDeltaOff, Just ptr@(PtrOfs doff)) -> do
                    let delta = deltaRead objData
                    let noffset = offset - doff
                    base <- resolve reader noffset =<< packReadRawAtOffset reader noffset
                    return $ addToChain ptr $ applyDelta delta base
                (TypeDeltaRef, Just ptr@(PtrRef bref)) -> do
                    let delta = deltaRead objData
                    base <- getObjectRaw git bref True
                    return $ addToChain ptr $ applyDelta delta base
                _ ->
                    return $ Just $ generifyHeader (po, objData)

        addToChain ptr (Just oi) = Just (oi { oiChains = ptr : oiChains oi })
        addToChain _   Nothing   = Nothing

        applyDelta :: Maybe Delta -> Maybe ObjectInfo -> Maybe ObjectInfo
        applyDelta (Just delta@(Delta _ rSize _)) (Just objInfo) = Just $ objInfo
            { oiHeader = (\(a,_,c) -> (a,rSize,c)) $ oiHeader objInfo
            , oiData   = deltaApply (oiData objInfo) delta
            }
        applyDelta _ _                                      = Nothing

-- | get an object from repository
getObjectRawAt :: Git -> ObjectLocation -> Bool -> IO (Maybe ObjectInfo)
getObjectRawAt _   NotFound    _ = return Nothing
getObjectRawAt git (Loose ref) _ = Just . (\(h,d)-> ObjectInfo h d[]) <$> looseReadRaw (gitRepoPath git) ref
getObjectRawAt git (Packed pref o) resolveDelta = readFromPack git pref o resolveDelta

-- | get an object from repository
getObjectRaw :: Git -> Ref -> Bool -> IO (Maybe ObjectInfo)
getObjectRaw git ref resolveDelta = do
    loc <- findReference git ref
    getObjectRawAt git loc resolveDelta

-- | get an object type from repository
getObjectType :: Git -> Ref -> IO (Maybe ObjectType)
getObjectType git ref = findReference git ref >>= getObjectTypeAt
  where getObjectTypeAt NotFound        = return Nothing
        getObjectTypeAt (Loose _)       = Just . (\(t,_,_) -> t) <$> looseReadHeader (gitRepoPath git) ref
        getObjectTypeAt (Packed pref o) =
            fmap ((\(ty,_,_) -> ty) . oiHeader) <$> readFromPack git pref o True

-- | get an object from repository using a location to reference it.
getObjectAt :: Git -> ObjectLocation -> Bool -> IO (Maybe Object)
getObjectAt git loc resolveDelta = maybe Nothing toObj <$> getObjectRawAt git loc resolveDelta
  where toObj (ObjectInfo { oiHeader = (ty, _, extra), oiData = objData }) = packObjectFromRaw (ty, extra, objData)

-- | get an object from repository using a ref.
getObject :: Git               -- ^ repository
          -> Ref               -- ^ the object's reference to
          -> Bool              -- ^ whether to resolve deltas if found
          -> IO (Maybe Object) -- ^ returned object if found
getObject git ref resolveDelta = maybe Nothing toObj <$> getObjectRaw git ref resolveDelta
  where toObj (ObjectInfo { oiHeader = (ty, _, extra), oiData = objData }) = packObjectFromRaw (ty, extra, objData)

-- | Just like 'getObject' but will raise a RefNotFound exception if the
-- reference cannot be found.
getObject_ :: Git       -- ^ repository
           -> Ref       -- ^ the object's reference to
           -> Bool      -- ^ whether to resolve deltas if found
           -> IO Object -- ^ returned object if found
getObject_ git ref resolveDelta = maybe (throwIO $ RefNotFound ref) return
                              =<< getObject git ref resolveDelta

-- | set an object in the store and returns the new ref
-- this is always going to create a loose object.
setObject :: Git
          -> Object
          -> IO Ref
setObject git obj = looseWrite (gitRepoPath git) obj
