-- |
-- Module      : Data.Git.Monad
-- License     : BSD-style
-- Maintainer  : Nicolas DI PRIMA <nicolas@di-prima.fr>
-- Stability   : experimental
-- Portability : unix
--
-- Simplifies the Git operation presents in this package.
--
-- You can easily access to the usual Git general informations:
--
-- * access to Head, Branches or Tags
-- * direct access to a Commit
--
-- This module also defines a convenient Monad to access the whole information
-- from a Commit: see 'CommitAccessMonad' and 'withCommit'.
--
-- You can also easily create a new commit: see 'CommitM' and 'withNewCommit'
--

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Git.Monad
    ( -- * GitMonad
      GitMonad(..)
    , GitM
    , withRepo
    , withCurrentRepo
      -- ** Operations
    , Resolvable(..)
    , branchList
    , branchWrite
    , tagList
    , tagWrite
    , headGet
    , headResolv
    , headSet
    , getCommit

      -- * Read a commit
    , CommitAccessM
    , withCommit
      -- ** Operations
    , getAuthor
    , getCommitter
    , getParents
    , getExtras
    , getEncoding
    , getMessage
    , getFile
    , getDir
      -- * Create a new Commit
    , CommitM
    , withNewCommit
    , withBranch
      -- ** Operations
    , setAuthor
    , setCommitter
    , setParents
    , setExtras
    , setEncoding
    , setMessage
    , setFile
    , deleteFile

      -- * convenients re-exports
    , Git.Git
    , Git.Ref
    , Git.RefName(..)
    , Git.Commit(..)
    , Git.Person(..)
    ) where


import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Git as Git
import qualified Data.Git.Revision as Git
import qualified Data.Git.Repository as Git
import qualified Data.Git.Storage.Object as Git
import           Data.Git.Imports

import qualified Filesystem.Path as FP

import Data.Set (Set)

-------------------------------------------------------------------------------
--                              Revision helper                              --
-------------------------------------------------------------------------------

revisionFromString :: String -> Git.Revision
revisionFromString = Git.fromString

-- | this is a convenient class to allow a common interface for what user may
-- need to optain a Ref from a given Resolvable object.
--
-- each of this instances is a convenient implementation of what a user would
-- have to do in order to resolve a branch, a tag or a String.
--
-- > resolve (Ref "2ad98b90...2ca") === Ref "2ad98b90...2ca"
-- > resolve "master"
-- > resolve "HEAD^^^"
--
class Resolvable rev where
    resolve :: GitMonad m => rev -> m (Maybe Git.Ref)
instance Resolvable Git.Ref where
    resolve = return . Just
instance Resolvable Git.Revision where
    resolve rev = do
        git <- getGit
        liftGit $ Git.resolveRevision git rev
instance Resolvable String where
    resolve = resolve . revisionFromString
instance Resolvable Git.RefName where
    resolve = resolve . Git.refNameRaw

-------------------------------------------------------------------------------
--                              GitMonad                                     --
-------------------------------------------------------------------------------

-- | Basic operations common between the different Monads defined in this
-- package.
class (Functor m, Applicative m, Monad m) => GitMonad m where
    -- | the current Monad must allow access to the current Git
    getGit :: m Git.Git
    liftGit :: IO a -> m a

branchList :: GitMonad git => git (Set Git.RefName)
branchList = getGit >>= liftGit . Git.branchList

branchWrite :: GitMonad git => Git.RefName -> Git.Ref -> git ()
branchWrite rn ref = do
    git <- getGit
    liftGit $ Git.branchWrite git rn ref

tagList :: GitMonad git => git (Set Git.RefName)
tagList = getGit >>= liftGit . Git.tagList

tagWrite :: GitMonad git => Git.RefName -> Git.Ref -> git ()
tagWrite rn ref = do
    git <- getGit
    liftGit $ Git.tagWrite git rn ref

headGet :: GitMonad git => git (Either Git.Ref Git.RefName)
headGet = getGit >>= liftGit . Git.headGet

headResolv :: GitMonad git => git (Maybe Git.Ref)
headResolv = do
    e <- headGet
    case e of
        Left ref -> resolve ref
        Right v  -> resolve v

headSet :: GitMonad git => Either Git.Ref Git.RefName -> git ()
headSet e = do
    git <- getGit
    liftGit $ Git.headSet git e

getCommit :: (GitMonad git, Resolvable ref) => ref -> git (Maybe Git.Commit)
getCommit r = do
    mRef <- resolve r
    case mRef of
        Nothing  -> return Nothing
        Just ref -> do
            git <- getGit
            liftGit $ Git.getCommitMaybe git ref

setObject :: (GitMonad git, Git.Objectable obj) => obj -> git Git.Ref
setObject obj = do
    git <- getGit
    liftGit $ Git.setObject git $ Git.toObject obj

getObject :: (GitMonad git, Resolvable ref)
          => ref
          -> Bool
          -> git (Maybe Git.Object)
getObject rev resolvDelta = do
    git <- getGit
    mRef <- resolve rev
    case mRef of
        Nothing  -> return Nothing
        Just ref -> liftGit $ Git.getObject git ref resolvDelta

workTreeNew :: GitMonad git => git Git.WorkTree
workTreeNew = liftGit Git.workTreeNew

workTreeFrom :: GitMonad git => Git.Ref -> git Git.WorkTree
workTreeFrom ref = liftGit $ Git.workTreeFrom ref

workTreeFlush :: GitMonad git => Git.WorkTree -> git Git.Ref
workTreeFlush tree = do
    git <- getGit
    liftGit $ Git.workTreeFlush git tree

resolvPath :: (GitMonad git, Resolvable ref)
           => ref -- ^ the commit Ref, Revision ("master", "HEAD^^" or a ref...)
           -> Git.EntPath
           -> git (Maybe Git.Ref)
resolvPath commitRev entPath = do
    git <- getGit
    mRef <- resolve commitRev
    case mRef of
        Nothing  -> return Nothing
        Just ref -> liftGit $ Git.resolvePath git ref entPath

-------------------------------------------------------------------------------
--                                                                           --
-------------------------------------------------------------------------------

data Result ctx a
    = ResultSuccess !ctx !a
    | ResultFailure !String

-------------------------------------------------------------------------------
--                                 GitM                                      --
-------------------------------------------------------------------------------

data GitContext = GitContext
    { gitContextGit  :: !Git.Git
    }

newtype GitM a = GitM
    { runGitM :: GitContext -> IO (Result GitContext a)
    }

instance Functor GitM where
    fmap = fmapGitM

instance Applicative GitM where
    pure  = returnGitM
    (<*>) = appendGitM

instance Monad GitM where
    return = returnGitM
    (>>=)  = bindGitM
    fail   = failGitM

instance GitMonad GitM where
    getGit  = getGitM
    liftGit = liftGitM

fmapGitM :: (a -> b) -> GitM a -> GitM b
fmapGitM f m = GitM $ \ctx -> do
    r <- runGitM m ctx
    return $ case r of
        ResultSuccess ctx' v -> ResultSuccess ctx' (f v)
        ResultFailure err    -> ResultFailure err

returnGitM :: a -> GitM a
returnGitM v = GitM $ \ctx -> return (ResultSuccess ctx v)

appendGitM :: GitM (a -> b) -> GitM a -> GitM b
appendGitM m1f m2 = m1f >>= \f -> m2 >>= \v -> return (f v)

bindGitM :: GitM a -> (a -> GitM b) -> GitM b
bindGitM m fm = GitM $ \ctx -> do
    r <- runGitM m ctx
    case r of
        ResultSuccess ctx' v -> runGitM (fm v) ctx'
        ResultFailure err    -> return (ResultFailure err)

failGitM :: String -> GitM a
failGitM msg = GitM $ \_ -> return (ResultFailure msg)

getGitM :: GitM Git.Git
getGitM = GitM $ \ctx -> return (ResultSuccess ctx (gitContextGit ctx))

liftGitM :: IO a -> GitM a
liftGitM f = GitM $ \ctx -> ResultSuccess ctx <$> f

executeGitM :: Git.Git -> GitM a -> IO (Either String a)
executeGitM git m = do
    r <- runGitM m $ GitContext git
    return $ case r of
        ResultSuccess _   v -> Right v
        ResultFailure err   -> Left err

withRepo :: FP.FilePath -> GitM a -> IO (Either String a)
withRepo repoPath m = Git.withRepo repoPath (\git -> executeGitM git m)

withCurrentRepo :: GitM a -> IO (Either String a)
withCurrentRepo m = Git.withCurrentRepo (\git -> executeGitM git m)


-------------------------------------------------------------------------------
--                             CommitAccessM                                 --
-------------------------------------------------------------------------------

data CommitAccessContext = CommitAccessContext
    { commitAccessContextCommit :: !Git.Commit
    , commitAccessContextRef    :: !Git.Ref
    }

-- | ReadOnly operations on a given commit
newtype CommitAccessM  a = CommitAccessM
    { runCommitAccessM :: forall git . GitMonad git => CommitAccessContext -> git (Result CommitAccessContext a)
    }

instance Functor CommitAccessM where
    fmap = fmapCommitAccessM

instance Applicative CommitAccessM where
    pure  = returnCommitAccessM
    (<*>) = appendCommitAccessM

instance Monad CommitAccessM where
    return = returnCommitAccessM
    (>>=)  = bindCommitAccessM
    fail   = failCommitAccessM

instance GitMonad CommitAccessM where
    getGit  = getCommitAccessM
    liftGit = liftCommitAccessM

fmapCommitAccessM :: (a -> b) -> CommitAccessM a -> CommitAccessM b
fmapCommitAccessM f m = CommitAccessM $ \ctx -> do
    r <- runCommitAccessM m ctx
    return $ case r of
        ResultSuccess ctx' v -> ResultSuccess ctx' (f v)
        ResultFailure err    -> ResultFailure err

returnCommitAccessM :: a -> CommitAccessM a
returnCommitAccessM v = CommitAccessM $ \ctx -> return (ResultSuccess ctx v)

appendCommitAccessM :: CommitAccessM (a -> b) -> CommitAccessM a -> CommitAccessM b
appendCommitAccessM m1f m2 = m1f >>= \f -> m2 >>= \v -> return (f v)

bindCommitAccessM :: CommitAccessM a -> (a -> CommitAccessM b) -> CommitAccessM b
bindCommitAccessM m fm = CommitAccessM $ \ctx -> do
    r <- runCommitAccessM m ctx
    case r of
        ResultSuccess ctx' v -> runCommitAccessM (fm v) ctx'
        ResultFailure err    -> return (ResultFailure err)

failCommitAccessM :: String -> CommitAccessM a
failCommitAccessM msg = CommitAccessM $ \_ -> return (ResultFailure msg)

getCommitAccessM :: CommitAccessM Git.Git
getCommitAccessM = CommitAccessM $ \ctx -> ResultSuccess ctx <$> getGit

liftCommitAccessM :: IO a -> CommitAccessM a
liftCommitAccessM f = CommitAccessM $ \ctx -> ResultSuccess ctx <$> (liftGit f)

-- Operations -----------------------------------------------------------------

withCommitAccessContext :: (CommitAccessContext -> a) -> CommitAccessM a
withCommitAccessContext operation = CommitAccessM $ \ctx ->
    return $ ResultSuccess ctx $ operation ctx

getAuthor :: CommitAccessM Git.Person
getAuthor = withCommitAccessContext (Git.commitAuthor . commitAccessContextCommit)

getCommitter :: CommitAccessM Git.Person
getCommitter = withCommitAccessContext (Git.commitCommitter . commitAccessContextCommit)

getParents :: CommitAccessM [Git.Ref]
getParents = withCommitAccessContext (Git.commitParents . commitAccessContextCommit)

getExtras :: CommitAccessM [Git.CommitExtra]
getExtras = withCommitAccessContext (Git.commitExtras . commitAccessContextCommit)

getEncoding :: CommitAccessM (Maybe ByteString)
getEncoding = withCommitAccessContext (Git.commitEncoding . commitAccessContextCommit)

getMessage :: CommitAccessM ByteString
getMessage = withCommitAccessContext (Git.commitMessage . commitAccessContextCommit)

getContextRef_ :: CommitAccessM Git.Ref
getContextRef_ = withCommitAccessContext commitAccessContextRef

getContextObject_ :: Git.EntPath -> CommitAccessM (Maybe Git.Object)
getContextObject_ fp = do
    commitRef <- getContextRef_
    mRef <- resolvPath commitRef fp
    case mRef of
        Nothing  -> return Nothing
        Just ref -> getObject ref True

-- | get the content of the file at the given Path
--
-- if the given Path is not a file or does not exist,
-- the function returns Nothing.
getFile :: Git.EntPath -> CommitAccessM (Maybe BL.ByteString)
getFile fp = do
    mObj <- getContextObject_ fp
    return $ case mObj of
        Nothing  -> Nothing
        Just obj -> case Git.objectToBlob obj of
            Nothing -> Nothing
            Just b  -> Just $ Git.blobGetContent b

-- | list the element present in the Given Directory Path
--
-- if the given Path is not a directory or does not exist,
-- the function returns Nothing.
getDir :: Git.EntPath -> CommitAccessM (Maybe [Git.EntName])
getDir fp = do
    mObj <- getContextObject_ fp
    return $ case mObj of
        Nothing  -> Nothing
        Just obj -> case Git.objectToTree obj of
            Nothing   -> Nothing
            Just tree -> Just $ map (\(_, n, _) -> n) $ Git.treeGetEnts tree

-- | open a commit in the current GitMonad
--
-- Read commit's info (Author, Committer, message...) or Commit's Tree.
--
-- > withCurrentRepo $
-- >    withCommit "master" $ do
-- >        -- print the commit's author information
-- >        author <- getAuthor
-- >        liftGit $ print author
-- >
-- >        -- print the list of files|dirs in the root directory
-- >        l <- getDir []
-- >        liftGit $ print l
--
withCommit :: (Resolvable ref, GitMonad git)
           => ref
                -- ^ the commit revision or reference to open
           -> CommitAccessM a
           -> git a
withCommit rev m = do
    mRef <- resolve rev
    case mRef of
        Nothing -> fail "revision does not exist"
        Just ref -> do
            mCommit <- getCommit ref
            case mCommit of
                Nothing -> fail $ "the given ref does not exist or is not a commit"
                Just commit -> do
                    let ctx = CommitAccessContext
                                { commitAccessContextCommit = commit
                                , commitAccessContextRef    = ref
                                }
                    r <- runCommitAccessM m ctx
                    case r of
                        ResultFailure err   -> fail err
                        ResultSuccess _   a -> return a

-------------------------------------------------------------------------------
--                                 CommitM                                      --
-------------------------------------------------------------------------------

data CommitContext = CommitContext
    { commitContextAuthor    :: !Git.Person
    , commitContextCommitter :: !Git.Person
    , commitContextParents   :: ![Git.Ref]
    , commitContextExtras    :: ![Git.CommitExtra]
    , commitContextEncoding  :: !(Maybe ByteString)
    , commitContextMessage   :: !ByteString
    , commitContextTree      :: !Git.WorkTree
    }

newtype CommitM  a = CommitM
    { runCommitM :: forall git . GitMonad git => CommitContext -> git (Result CommitContext a)
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

instance GitMonad CommitM where
    getGit  = getCommitM
    liftGit = liftCommitM

fmapCommitM :: (a -> b) -> CommitM a -> CommitM b
fmapCommitM f m = CommitM $ \ctx -> do
    r <- runCommitM m ctx
    return $ case r of
        ResultSuccess ctx' v -> ResultSuccess ctx' (f v)
        ResultFailure err    -> ResultFailure err

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
failCommitM msg = CommitM $ \_ -> return (ResultFailure msg)

getCommitM :: CommitM Git.Git
getCommitM = CommitM $ \ctx -> ResultSuccess ctx <$> getGit

liftCommitM :: IO a -> CommitM a
liftCommitM f = CommitM $ \ctx -> ResultSuccess ctx <$> (liftGit f)

-- Operations -----------------------------------------------------------------

commitUpdateContext :: (CommitContext -> IO (CommitContext, a)) -> CommitM a
commitUpdateContext operation = CommitM $ \ctx -> do
    (ctx', r) <- liftGit $ operation ctx
    return (ResultSuccess ctx' r)

-- | replace the Commit's Author
setAuthor :: Git.Person -> CommitM ()
setAuthor p = commitUpdateContext $ \ctx -> return (ctx { commitContextCommitter = p }, ())

-- | replace the Commit's Committer
setCommitter :: Git.Person -> CommitM ()
setCommitter p = commitUpdateContext $ \ctx -> return (ctx { commitContextCommitter = p }, ())

-- | replace the Commit's Parents
setParents :: [Git.Ref] -> CommitM ()
setParents l = commitUpdateContext $ \ctx -> return (ctx { commitContextParents = l }, ())

-- | replace the Commit's Extras
setExtras :: [Git.CommitExtra] -> CommitM ()
setExtras l = commitUpdateContext $ \ctx -> return (ctx { commitContextExtras = l }, ())

-- | replace the Commit's encoding
setEncoding :: Maybe ByteString -> CommitM ()
setEncoding e = commitUpdateContext $ \ctx -> return (ctx { commitContextEncoding = e }, ())

-- | replace the Commit's message with the new given message.
setMessage :: ByteString -> CommitM ()
setMessage msg = commitUpdateContext $ \ctx -> return (ctx { commitContextMessage = msg }, ())

setContextObject_ :: Git.Objectable object
                  => Git.EntPath
                  -> (Git.EntType, object)
                  -> CommitM ()
setContextObject_ path (t, obj) = do
    ref <- setObject obj
    git <- getGit
    commitUpdateContext $ \ctx -> do
        Git.workTreeSet git (commitContextTree ctx) path (t, ref)
        return (ctx, ())

-- | add a new file in in the Commit's Working Tree
setFile :: Git.EntPath
        -> BL.ByteString
        -> CommitM ()
setFile path bl = setContextObject_ path (Git.EntFile , Git.Blob bl)

-- | delete a file from the Commit's Working Tree.
deleteFile :: Git.EntPath -> CommitM ()
deleteFile path = do
    git <- getGit
    commitUpdateContext $ \ctx -> do
        Git.workTreeDelete git (commitContextTree ctx) path
        return (ctx, ())

-- | create a new commit in the current GitMonad
--
-- The commit is pre-filled with the following default values:
--
-- * author and committer are the same
-- * the commit's parents is an empty list
-- * there is no commit encoding
-- * the commit's extras is an empty list
-- * the commit message is an empty ByteString
-- * the working tree is a new empty Tree or the Tree associated to the
--   given Revision or Ref.
--
-- You can update these values with the commit setters (setFile, setAuthor...)
--
-- Example:
--
-- > withCurrentRepo $ 
-- >    (r, ()) <- withNewCommit person Nothing $ do
-- >        setMessage "inital commit"
-- >        setFile ["README.md"] "# My awesome project\n\nthis is a new project\n"
-- >    branchWrite "master" r
-- >
--
-- you can also continue the work on a same branch. In this case the commit's
-- parent is already set to the Reference associated to the revision.
-- You can, change the parents if you wish to erase, or replace, this value.
--
-- > withCurrentRepo $
-- >    readmeContent <- withCommit (Just "master") $ getFile ["README.md"]
-- >    (r, ()) <- withNewCommit person (Just "master") $ do
-- >        setMessage "update the README"
-- >        setFile ["README.md"] $ readmeContent <> "just add some more description\n"
-- >    branchWrite "master" r
--
withNewCommit :: (GitMonad git, Resolvable rev)
              => Git.Person
                -- ^ by default a commit must have an Author and a Committer.
                --
                -- The given value will be given to both Author and Committer.
              -> Maybe rev
                -- ^ it is possible to prepopulate the Working Tree with a
                -- given Ref's Tree.
              -> CommitM a
                -- ^ the action to perform in the new commit (set files,
                -- Person, encoding or extras)
              -> git (Git.Ref, a)
withNewCommit p mPrec m = do
    workTree <- case mPrec of
                    Nothing -> workTreeNew
                    Just r -> do
                        mc <- getCommit r
                        case mc of
                            Nothing -> fail "the given revision does not exist or is not a commit"
                            Just c  -> workTreeFrom (Git.commitTreeish c)
    parents <- case mPrec of
                    Nothing -> return []
                    Just r  -> do
                        mr <- resolve r
                        return $ case mr of
                            Nothing -> []
                            Just ref -> [ref]
    let ctx = CommitContext
                { commitContextAuthor    = p
                , commitContextCommitter = p
                , commitContextParents   = parents
                , commitContextExtras    = []
                , commitContextEncoding  = Nothing
                , commitContextMessage   = B.empty
                , commitContextTree      = workTree
                }
    r <- runCommitM m ctx
    case r of
        ResultFailure err    -> fail err
        ResultSuccess ctx' a -> do
            treeRef <- workTreeFlush (commitContextTree ctx')
            let commit = Git.Commit
                            { Git.commitTreeish   = treeRef
                            , Git.commitParents   = commitContextParents ctx'
                            , Git.commitAuthor    = commitContextAuthor ctx'
                            , Git.commitCommitter = commitContextCommitter ctx'
                            , Git.commitEncoding  = commitContextEncoding ctx'
                            , Git.commitExtras    = commitContextExtras ctx'
                            , Git.commitMessage   = commitContextMessage ctx'
                            }
            ref <- setObject commit
            return (ref, a)

-- | create or continue to work on a branch
--
-- This is a convenient function to create or to linearily work  on a branch.
-- This function applies a first Collect of information on the parent commit
-- (the actual branch's commit). Then it creates a new commit and update
-- the branch to point to this commit.
--
-- for example:
--
-- @
-- withCurrentRepo $
--     withBranch person "master" True
--         (getAuthor)
--         (maybe (setMessage "initial commit on this branch")
--                (\author -> setMessage $ "continue the great work of " ++ show (personName author))
--         )
-- @
--
withBranch :: GitMonad git
           => Git.Person
                -- ^ the default Author and Committer (see 'withNewCommit')
           -> Git.RefName
                -- ^ the branch to work on
           -> Bool
                -- ^ propopulate the parent's tree (if it exists) in the
                -- new created commit.
                --
                -- In any cases, if the branch already exists, the new commit
                -- parent will be filled with the result of ('resolv' "branchName")
           -> (CommitAccessM a)
                -- ^ the action to performs in the parent's new commit if it exists.
           -> (Maybe a -> CommitM b)
                -- ^ the action to performs in the new commit
                --
                -- the argument is the result of the action on the parent commit.
                --
                -- Nothing if the parent does not exist.
           -> git (Git.Ref, b)
withBranch p branchName keepTree actionParent actionNew = do
    -- attempt to resolve the branch
    mRefParent <- resolve branchName

    -- configure the precedency of the tree and the action in the new commit
    (mRefTree, actionInCommit) <- case mRefParent of
        -- in the case the branch does not exist already: there is not precedency
        Nothing -> return (Nothing, actionNew Nothing)
        -- if the branch exists
        Just refParent -> do
            -- performs the action in the parent commit
            a <- withCommit refParent actionParent
            return $ if keepTree
                -- if user has choosen to prepopulate the Tree with the
                -- parent's tree we prepopulate the tree.
                then (Just refParent, actionNew $ Just a)
                -- else, we make sure the parent is at least setted
                else (Nothing, setParents [refParent] >> actionNew (Just a))
    -- create the new commit
    (ref, b) <- withNewCommit p (mRefTree) actionInCommit
    -- write the branch
    branchWrite branchName ref
    return (ref, b)
