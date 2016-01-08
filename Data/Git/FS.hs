-- |
-- Module      : Data.Git.FS
-- License     : BSD-style
-- Maintainer  : Nicolas DI PRIMA <nicolas@di-prima.fr>
-- Stability   : experimental
-- Portability : unix
--
-- Manipulate a git repository as a FileSystem. Thid module allows user
-- to modify/create git Files in the context of a new commit.
--
-- This module uses the tool already present in this package to allow a
-- easy and naive API: you can only do the basic git manipulation.
--
-- The following example shows how to initialise a new git repository
-- with the current working directory.
--
-- > example :: IO ()
-- > example = do
-- >     initRepo ".git"
-- >     withRepo ".git" $ \git -> do
-- >         tz      <- timezoneCurrent
-- >         current <- timeCurrent
-- >         let person = Person
-- >                 { personName  = "User Name"
-- >                 , personEmail = "user@example.com"
-- >                 , personTime  = GitTime current tz
-- >                 }
-- >         _ <- withNewCommit_ git person (CommitPrecedentDir ".") "initial commit" $
-- >                  setBranch (Just $ RefName "master")
-- >         headSet git (Right "master")
-- >         return ()
--
-- And the following example shows to update the given branch by removing the
-- '*.o' files that has been added by mistake in the previous commit:
--
-- > example :: IO ()
-- > example = do
-- >     withRepo ".git" $ \git -> do
-- >         tz      <- timezoneCurrent
-- >         current <- timeCurrent
-- >         let person = Person
-- >                 { personName  = "User Name"
-- >                 , personEmail = "user@example.com"
-- >                 , personTime  = GitTime current tz
-- >                 }
-- >         _ <- withNewCommit_ git person (CommitPrecedentBranch "master") "remove C object files" $
-- >                l <- listFiles
-- >                mapM_ deleteFile $ Data.List.filter (\f -> maybe False ((==) (.o)) $ Filesystem.Path.extension f) l
-- >         return ()
--

{-# LANGUAGE OverloadedStrings #-}

module Data.Git.FS {-# DEPRECATED "Data.Git.Monad instead" #-}
    ( -- * Repo
      Git
    , withRepo
    , withCurrentRepo

      -- * Commit
    , CommitM
    , withCommit
    , withNewCommit
    , withNewCommit_
    , CommitPrecedent(..)

      -- ** Operations
    , setAuthor
    , getAuthor
    , setCommitter
    , getCommitter
    , setParents
    , getParents
    , setEncoding
    , getEncoding
    , setExtras
    , getExtras
    , setBranch
    , getMessage
    , getRef
    , readFile
    , writeFile
    , appendFile
    , deleteFile
    , listFiles
    ) where

import Prelude hiding (FilePath, readFile, writeFile, appendFile)

import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.UTF8 (fromString)
import Data.List (sortBy)
import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as M
import Data.STRef

import qualified Filesystem as F
import Filesystem.Path hiding (root)
import Filesystem.Path.CurrentOS hiding (root)

import Data.Git
import Data.Git.Repository (buildHTree, HTree, HTreeEnt(..), getCommitMaybe)
import Data.Git.Storage
import Data.Git.Storage.Object
import Data.Git.Types
import qualified Data.Git.Revision as Rev

-------------------------------------------------------------------------------
--                              CommitM                                      --
-------------------------------------------------------------------------------

data CommitPrecedent
    = CommitPrecedent Ref
        -- ^ use the reference of a commit to pre-populate a Tree with this
        -- commit Tree. The files present in this commit's tree will be
        -- available for manipulation/update and will be associated to the
        -- new commit.
        --
        -- The Ref will be automatically added to the new commit's parent
        -- list. You can disable this with "setParents []".
    | CommitPrecedentBranch RefName
        -- ^ use a RefName (a branch, a tag, ...) to prepopulate the Tree of
        -- the new commit. The commit associated to this RefName will be added
        -- in the new commit's parent list. and this RefName will be updated
        -- to point to the new commit.
        --
        -- you can disable the branch update with "setBranch"
    | CommitPrecedentDir FilePath
        -- ^ Use a directory for the given commit
        --
        -- all the files under this directory will be included in the
        -- new commit (and will be accessible for update/modification)
        -- in the new commit context.
    | CommitPrecedentEmpty
        -- ^ start a new commit with no files already included into
        -- the commit Tree, no Ref in the parents list and no RefName to update.

data TreeStatus
    = TreeNotLoaded Ref
        -- ^ the ref of a Tree that has not been loaded
    | TreeReadOnly  Ref BlobsMap
        -- ^ the ref of a Tree and the BlobsMap
        -- no modification has been performed to the blobs map
    | TreeReadWrite BlobsMap
        -- ^ a new blobs map

commitPrecedentToBlobsMap :: Git -> CommitPrecedent -> IO (Either String TreeStatus)
commitPrecedentToBlobsMap git cp = case cp of
    CommitPrecedentEmpty -> return $ Right $ TreeReadWrite M.empty
    CommitPrecedent ref  -> do
        mCommit <- getCommitMaybe git ref
        return $ case mCommit of
            Nothing     -> Left "commit not found"
            Just commit -> Right $ TreeNotLoaded (commitTreeish commit)
    CommitPrecedentBranch refname -> do
        mCRef <- resolveRevision git $ Rev.fromString $ refNameRaw refname
        case mCRef of
            Nothing   -> return $ Left $ "branch not resolved: " ++ refNameRaw refname
            Just cRef -> commitPrecedentToBlobsMap git (CommitPrecedent cRef)
    CommitPrecedentDir dir -> do
        l <- listFilesFromDir dir
        l' <- mapM createRefFromFilePath l
        return $ Right $ TreeReadWrite $ M.fromList l'
  where
    listFilesFromDir :: FilePath -> IO [FilePath]
    listFilesFromDir fp = do
        isDir <- F.isDirectory fp
        case isDir of
            True -> do
                l <- F.listDirectory fp
                l' <- mapM listFilesFromDir l
                return $ L.concat l'
            False -> return [fp]

    createRefFromFilePath :: FilePath -> IO (FilePath, Ref)
    createRefFromFilePath fp = do
        blob <- Blob <$> BL.readFile (encodeString fp)
        ref <- setObject git $ toObject blob
        return (fp, ref)

data Result a
    = ResultSuccess CommitContext a
    | ResultFailure String

data CommitContext = CommitContext
    { commitContextGit       :: Git
    , commitContextRef       :: Maybe Ref
    , commitContextTree      :: TreeStatus
    , commitContextAuthor    :: Person
    , commitContextCommitter :: Person
    , commitContextParents   :: [Ref]
    , commitContextExtras    :: [CommitExtra]
    , commitContextEncoding  :: Maybe ByteString
    , commitContextBranch    :: Maybe RefName
    , commitContextMessage   :: ByteString
    }

defaultCommitContext :: Git -> Maybe Ref -> Person -> TreeStatus -> Maybe RefName -> [Ref] -> CommitContext
defaultCommitContext git mRef p b mBranch parents = CommitContext
    { commitContextGit       = git
    , commitContextRef       = mRef
    , commitContextTree      = b
    , commitContextAuthor    = p
    , commitContextCommitter = p
    , commitContextParents   = parents
    , commitContextExtras    = []
    , commitContextEncoding  = Nothing
    , commitContextBranch    = mBranch
    , commitContextMessage   = ""
    }

newtype CommitM a = CommitM
    { runCommitM :: CommitContext -> IO (Result a)
    }

instance Functor CommitM where
    fmap = fmapCommitM

instance Applicative CommitM where
    pure  = returnCommitM
    (<*>) = appendCommitM

instance Monad CommitM where
    return = returnCommitM
    (>>=)  = bindCommitM
    fail   = failCommitM

instance Alternative CommitM where
    empty = fail "Atlternative.empty"
    (<|>) = alternativeCommitM

alternativeCommitM :: CommitM a -> CommitM a -> CommitM a
alternativeCommitM m1 m2 = CommitM $ \ctx -> do
    r <- runCommitM m1 ctx
    case r of
        ResultSuccess ctx' v -> return (ResultSuccess ctx' v)
        ResultFailure _      -> runCommitM m2 ctx

fmapCommitM :: (a -> b) -> CommitM a -> CommitM b
fmapCommitM f m = CommitM $ \ctx -> do
    r <- runCommitM m ctx
    case r of
        ResultSuccess ctx' v -> return (ResultSuccess ctx' (f v))
        ResultFailure err    -> return (ResultFailure err)

returnCommitM :: a -> CommitM a
returnCommitM v = CommitM $ \ctx -> return (ResultSuccess ctx v)

appendCommitM :: CommitM (a -> b) -> CommitM a -> CommitM b
appendCommitM m1f m2 = m1f >>= \f -> m2 >>= \v -> return (f v)

bindCommitM :: CommitM a -> (a -> CommitM b) -> CommitM b
bindCommitM m fm = CommitM $ \ctx -> do
    r <- runCommitM m ctx
    case r of
        ResultSuccess ctx' v -> runCommitM (fm v) ctx'
        ResultFailure err    -> return (ResultFailure err)

failCommitM :: String -> CommitM a
failCommitM msg = CommitM $ \_ -> return $ ResultFailure msg

-- | this function allows user to view a git commit in its context
-- (i.e. accessing the git commit in a ReadOnly mode: commit message,
-- authors, files...)
withCommit :: Git
           -> CommitPrecedent
           -> CommitM a
           -> IO (Either String a)
withCommit git prec m = do
    mRef <- defaultParents
    case mRef of
        Nothing  -> return $ Left "cannot load the given commit"
        Just ref -> do
            mCommit <- getCommitMaybe git ref
            case mCommit of
                Nothing -> return $ Left "cannot load the given commit"
                Just commit -> do
                    let ctx = CommitContext
                                { commitContextGit       = git
                                , commitContextRef       = mRef
                                , commitContextTree      = TreeNotLoaded (commitTreeish commit)
                                , commitContextAuthor    = commitAuthor commit
                                , commitContextCommitter = commitCommitter commit
                                , commitContextParents   = commitParents commit
                                , commitContextExtras    = commitExtras commit
                                , commitContextEncoding  = commitEncoding commit
                                , commitContextBranch    = mBranch
                                , commitContextMessage   = commitMessage commit
                                }
                    r <- runCommitM m ctx
                    return $ case r of
                        ResultFailure err   -> Left err
                        ResultSuccess _   a -> Right a
  where
    mBranch :: Maybe RefName
    mBranch = case prec of
        CommitPrecedentBranch ref -> Just ref
        _                         -> Nothing
    defaultParents :: IO (Maybe Ref)
    defaultParents = case prec of
        CommitPrecedent ref -> return $ Just ref
        CommitPrecedentBranch refname -> do
            resolveRevision git $ Rev.fromString $ refNameRaw refname
        _ -> return Nothing

-- | this function creates a new commit with the given information
withNewCommit :: Git
                    -- ^ the git to use to create/manipulate the new commit in
              -> Person
                    -- ^ the Person to use (will be set as Author and Committer)
                    -- use setAuthor or setCommitter to update these values.
              -> CommitPrecedent
              -> ByteString
                    -- ^ the commit message
              -> CommitM a
                    -- ^ the operation to person in the commit
              -> IO (Either String (Ref, a))
withNewCommit git person prec msg m = do
    eb <- commitPrecedentToBlobsMap git prec
    case eb of
        Left err -> return $ Left $ "cannot initialized Commit context: " ++ err
        Right b  -> do
            parents <- defaultParents
            r <- runCommitM m (defaultCommitContext git Nothing person b mBranch parents)
            case r of
                ResultFailure err   -> return $ Left err
                ResultSuccess ctx a -> do
                    tree <- case commitContextTree ctx of
                                TreeNotLoaded ref   -> return ref
                                TreeReadOnly  ref _ -> return ref
                                TreeReadWrite bm    -> makeTrees git bm
                    let commit = Commit
                                    { commitTreeish   = tree
                                    , commitParents   = commitContextParents ctx
                                    , commitAuthor    = commitContextAuthor ctx
                                    , commitCommitter = commitContextCommitter ctx
                                    , commitEncoding  = commitContextEncoding ctx
                                    , commitExtras    = commitContextExtras ctx
                                    , commitMessage   = msg
                                    }
                    ref <- setObject git (toObject commit)
                    case commitContextBranch ctx of
                        Nothing -> return ()
                        Just br -> branchWrite git br ref
                    return $ Right (ref, a)
  where
    mBranch :: Maybe RefName
    mBranch = case prec of
        CommitPrecedentBranch ref -> Just ref
        _                         -> Nothing
    defaultParents :: IO [Ref]
    defaultParents = case prec of
        CommitPrecedent ref -> return $ [ref]
        CommitPrecedentBranch refname -> do
            mCRef <- resolveRevision git $ Rev.fromString $ refNameRaw refname
            return $ case mCRef of
                Nothing  -> []
                Just ref -> [ref]
        _ -> return []

-- | same as withNewCommit but ignore the returned value
withNewCommit_ :: Git -> Person -> CommitPrecedent -> ByteString -> CommitM a -> IO (Either String Ref)
withNewCommit_ git p cp msg m = do
    r <- withNewCommit git p cp msg m
    return $ case r of
        Left  err      -> Left err
        Right (ref, _) -> Right ref

-------------------------------------------------------------------------------
--                               CommitM Operations                          --
-------------------------------------------------------------------------------

-- this function allow modification/manipulation of the information
-- in the context of the Commit
withContext :: (CommitContext -> IO (CommitContext, a)) -> CommitM a
withContext f = CommitM $ \ctx -> do
    (ctx', v) <- f ctx
    return $ ResultSuccess ctx' v

withContextBlobs :: (Git -> BlobsMap -> IO (BlobsMap, a)) -> CommitM a
withContextBlobs f = withContext $ \ctx -> do
    let git = commitContextGit ctx
    bm <- case commitContextTree ctx of
            TreeNotLoaded ref -> do
                htree <- buildHTree git =<< getTree git ref
                return $ blobsMapFromHTree htree
            TreeReadOnly _ bm -> return bm
            TreeReadWrite  bm -> return bm
    (bm', v) <- f git bm
    return (ctx {commitContextTree = TreeReadWrite bm' }, v)

withContextBlobsReadOnly :: (Git -> BlobsMap -> IO a) -> CommitM a
withContextBlobsReadOnly f = withContext $ \ctx -> do
    let git = commitContextGit ctx
    case commitContextTree ctx of
        TreeNotLoaded ref -> do
            htree <- buildHTree git =<< getTree git ref
            let bm = blobsMapFromHTree htree
            v <- f git bm
            return (ctx {commitContextTree = TreeReadOnly ref bm }, v)
        TreeReadOnly _ bm -> do
            v <- f git bm
            return (ctx, v)
        TreeReadWrite bm -> do
            v <- f git bm
            return (ctx, v)

-- | update the commit Author with the new given entry
-- by default the author is the same as the committer (see withNewCommit)
setAuthor :: Person -> CommitM ()
setAuthor p = withContext $ \ctx -> return $ (ctx { commitContextAuthor = p }, ())

-- | get the Author of the actual commit
getAuthor :: CommitM Person
getAuthor = withContext $ \ctx -> return (ctx, commitContextAuthor ctx)

-- | update the commit Committer with the new given entry
-- by default the committer is the same as the author (see withNewCommit)
setCommitter :: Person -> CommitM ()
setCommitter p = withContext $ \ctx -> return $ (ctx { commitContextCommitter = p }, ())

-- | get the Committer of the actual commit
getCommitter :: CommitM Person
getCommitter = withContext $ \ctx -> return (ctx, commitContextCommitter ctx)

-- | set the Parents of the commit (by default the list is empty -- the commit
-- has no parents)
--
-- this function erases the existing parents
setParents :: [Ref] -> CommitM ()
setParents lParents = withContext $ \ctx -> return $ (ctx { commitContextParents = lParents }, ())

-- | get the parents's ref of the actual commit
getParents :: CommitM [Ref]
getParents = withContext $ \ctx -> return (ctx, commitContextParents ctx)

-- | set extras information to the commit (by default this list is empty)
--
-- this function erases the existing extras
setExtras :: [CommitExtra] -> CommitM ()
setExtras p = withContext $ \ctx -> return $ (ctx { commitContextExtras = p }, ())

getExtras :: CommitM [CommitExtra]
getExtras = withContext $ \ctx -> return (ctx, commitContextExtras ctx)

-- | set the encoding (by default this value is Nothing)
setEncoding :: Maybe ByteString -> CommitM ()
setEncoding p = withContext $ \ctx -> return $ (ctx { commitContextEncoding = p }, ())

getEncoding :: CommitM (Maybe ByteString)
getEncoding = withContext $ \ctx -> return (ctx, commitContextEncoding ctx)

-- | set the Branch (by default this value is Nothing)
--
-- if there is a branch setted, the Branch will be created (or updated) with
-- the new commit reference.
setBranch :: Maybe RefName -> CommitM ()
setBranch p = withContext $ \ctx -> return $ (ctx { commitContextBranch = p }, ())

-- | get the commit's message
getMessage :: CommitM ByteString
getMessage = withContext $ \ctx -> return (ctx, commitContextMessage ctx)

getRef :: CommitM (Maybe Ref)
getRef = withContext $ \ctx -> return (ctx, commitContextRef ctx)

-- | read the content of the given file
--
-- return nothing if there is no file
readFile :: FilePath -> CommitM (Maybe BL.ByteString)
readFile fp = withContextBlobsReadOnly $ \git bm -> do
    case M.lookup fp bm of
        Nothing  -> return Nothing
        Just ref -> do
            content <- getContent git fp ref
            return $ Just content

-- | write the given content in the given filepath
--
-- if there was already a file present, it will be replace
-- with the given content
writeFile :: FilePath -> BL.ByteString -> CommitM ()
writeFile fp content = withContextBlobs $ \git bm -> do
    bm' <- createBlobs git [ (fp, content) ]
    return (M.union bm' bm, ())

-- | append the given content to the given file
-- if the file did not exist, the file will be created with
-- the given content
appendFile :: FilePath -> BL.ByteString -> CommitM ()
appendFile fp content' = do
    content <- maybe BL.empty id <$> readFile fp
    writeFile fp (BL.append content content')

-- | delete the given filepath from the commit
-- (do nothing if the filepath does not exist)
deleteFile :: FilePath -> CommitM ()
deleteFile fp = withContextBlobs $ \_ bm -> return (M.delete fp bm, ())

-- | list all the files present in this commit
listFiles :: CommitM [FilePath]
listFiles = withContextBlobsReadOnly $ \_ bm -> return $ map fst $ M.toList bm

-------------------------------------------------------------------------------
--                          Internal Hit Stuff                               --
-------------------------------------------------------------------------------

data HierST a = DirST (STRef a [(String, HierST a)])
              | EntST Ref
data Hier = Dir [(String, Hier)] | Ent Ref
    deriving (Show,Eq)
type BlobsMap = Map FilePath Ref

blobsMapFromHTree :: HTree -> BlobsMap
blobsMapFromHTree l = blobsMapFromHTree_ "" l M.empty

blobsMapFromHTree_ :: FilePath -> HTree -> BlobsMap -> BlobsMap
blobsMapFromHTree_ _ [] m = m
blobsMapFromHTree_ dir ((_, name, treeEnt):xs) m =
    blobsMapFromHTree_ dir xs $ case treeEnt of
        TreeDir _ stree -> blobsMapFromHTree_ fp stree m
        TreeFile r      -> M.insert fp r m
  where
    fp :: FilePath
    fp = dir </> (decodeString $ show name)

makeTrees :: Git -> BlobsMap -> IO Ref
makeTrees git blobMap = do
    let hiers = runST $ do
                    root <- DirST <$> newSTRef []
                    mapM_ (makeHierarchy root) $ map (\(k, v) -> (encodeString k, v)) $ M.toList blobMap
                    unwrapST root
    createTrees hiers
  where
    createTrees :: Hier -> IO Ref
    createTrees (Dir l) = mapM createTreeEnt l >>= createTree git . sortEnts
    createTrees _       = error "cannot create tree of a single blob"

    -- we need to map Dir to ref and Ent to ref.
    -- to create tree we need TreeEnts. treeents need names.

    -- recursively create ents.
    createTreeEnt (s, hier) = do
        case hier of
            Ent r -> return $ toTreeEnt False s r
            Dir l -> do
                ents <- sortEnts <$> mapM createTreeEnt l
                ref  <- createTree git ents
                return $ toTreeEnt True s ref

    sortEnts = sortBy (\(_,x,_) (_,y,_) -> compare x y)

    makeHierarchy acc (path,blobRef) =
        let spath = wordsBy (== '/') path
        in  addHier acc (spath,blobRef)
    unwrapST (EntST r) = return (Ent r)
    unwrapST (DirST r) = do
        l <- readSTRef r
        Dir <$> mapM (\(p, h) -> unwrapST h >>= \z -> return (p, z)) l

    addHier _    ([],_)   = error "empty path"
    addHier hier (list,e) = dive hier list
      where -- dive into the hierarchy
        dive (EntST _) _      = error "shouldn't dive in an ent"
        dive (DirST _) []     = error "internal error cannot happen."
        dive (DirST r) [fn]   = modifySTRef r $ \l -> (fn, EntST e) : l
        dive (DirST r) (x:xs) = readSTRef r >>= findOrAdd
          where
            findOrAdd dirL =
                case lookup x dirL of
                    Nothing -> do -- we need to create a new entry
                        nEnt <- DirST <$> newSTRef []
                        dive nEnt xs
                        writeSTRef r ((x, nEnt) : dirL)
                    Just childDir -> dive childDir xs

createBlobs :: Git
            -> [(FilePath, BL.ByteString)]
            -> IO BlobsMap
createBlobs git indexedContents = foldM createAndAcc M.empty indexedContents
  where
    createAndAcc acc (index, content) = do
        blob <- createBlob git content
        return $ M.insert index blob acc

createBlob :: Git -> BL.ByteString -> IO Ref
createBlob git content = setObject git (toObject $ Blob content)

createTree :: Git -> [TreeEnt] -> IO Ref
createTree git treeEnts = setObject git (toObject $ Tree treeEnts)

toTreeEnt :: Bool -> String -> Ref -> TreeEnt
toTreeEnt isDir name ref = (ModePerm perm, entName $ utf8Encode name, ref)
  where perm = if isDir then 0o040000 else 0o100644
        utf8Encode = fromString

-- | 'wordsBy' breaks a string up into a list of words, which were delimited
--  by a callback function that return true.
wordsBy :: (Char -> Bool) -> String -> [String]
wordsBy _ [] = [""]
wordsBy f s  =
    case break f s of
        (r,[])    -> [r]
        (r1,_:r2) -> r1 : wordsBy f r2

getContent :: Git -> FilePath -> Ref -> IO BL.ByteString
getContent git fp ref = do
    obj <- getObjectRaw git ref True
    case obj of
        Nothing -> fail $ "getContent: cannot get raw object of " ++ show fp
        Just v  -> return $ oiData v
