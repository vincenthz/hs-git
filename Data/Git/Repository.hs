{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Module      : Data.Git.Repository
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unix
--
module Data.Git.Repository
    ( Git
    -- * Config
    , configGetAll
    , configGet
    , Config(..)
    , Section(..)
    -- * Trees
    , HTree
    , HTreeEnt(..)
    , RefName(..)
    , getCommitMaybe
    , getCommit
    , getTreeMaybe
    , getTree
    , rewrite
    , buildHTree
    , resolvePath
    , resolveTreeish
    , resolveRevision
    , initRepo
    , isRepo
    -- * named refs manipulation
    , branchWrite
    , branchList
    , tagWrite
    , tagList
    , headSet
    , headGet
    ) where

import Control.Exception (Exception, throw)

import Data.Maybe (fromMaybe)
import Data.List (find)
import Data.Data
import Data.IORef

import Data.Git.Named
import Data.Git.Types
import Data.Git.Imports
import Data.Git.Storage.Object
import Data.Git.Storage
import Data.Git.Revision
import Data.Git.Storage.Loose
import Data.Git.Storage.CacheFile
import Data.Git.Ref
import Data.Git.Config (Config(..), Section(..))
import qualified Data.Git.Config as Cfg

import Data.Set (Set)

import qualified Data.Map as M
import qualified Data.Set as Set

-- | hierarchy tree, either a reference to a blob (file) or a tree (directory).
data HTreeEnt = TreeDir Ref HTree | TreeFile Ref
type HTree = [(ModePerm,EntName,HTreeEnt)]

-- | Exception when trying to convert an object pointed by 'Ref' to
-- a type that is different
data InvalidType = InvalidType Ref ObjectType
                 deriving (Show,Eq,Data,Typeable)

instance Exception InvalidType

-- should be a standard function that do that...
mapJustM f (Just o) = f o
mapJustM _ Nothing  = return Nothing

-- | get a specified commit
getCommitMaybe :: Git -> Ref -> IO (Maybe Commit)
getCommitMaybe git ref = maybe Nothing objectToCommit <$> getObject git ref True

-- | get a specified commit but raises an exception if doesn't exists or type is not appropriate
getCommit :: Git -> Ref -> IO Commit
getCommit git ref = maybe err id . objectToCommit <$> getObject_ git ref True
  where err = throw $ InvalidType ref TypeCommit

-- | get a specified tree
getTreeMaybe :: Git -> Ref -> IO (Maybe Tree)
getTreeMaybe git ref = maybe Nothing objectToTree <$> getObject git ref True

-- | get a specified tree but raise
getTree :: Git -> Ref -> IO Tree
getTree git ref = maybe err id . objectToTree <$> getObject_ git ref True
  where err = throw $ InvalidType ref TypeTree

-- | try to resolve a string to a specific commit ref
-- for example: HEAD, HEAD^, master~3, shortRef
resolveRevision :: Git -> Revision -> IO (Maybe Ref)
resolveRevision git (Revision prefix modifiers) =
    getCacheVal (packedNamed git) >>= \c -> resolvePrefix c >>= maybe (return Nothing) (modf modifiers)
  where
        resolvePrefix lookupCache = tryResolvers
              [resolveNamedPrefix lookupCache namedResolvers
              ,resolvePrePrefix
              ]

        resolveNamedPrefix _           []     = return Nothing
        resolveNamedPrefix lookupCache (x:xs) = followToRef (resolveNamedPrefix lookupCache xs) x
          where followToRef onFailure refty = do
                    exists <- existsRefFile (gitRepoPath git) refty
                    if exists
                        then do refcont <- readRefFile (gitRepoPath git) refty
                                case refcont of
                                     RefDirect ref     -> return $ Just ref
                                     RefLink refspecty -> followToRef onFailure refspecty
                                     _                 -> error "cannot handle reference content"
                        else case refty of
                                RefTag name    -> mapLookup name $ packedTags lookupCache
                                RefBranch name -> mapLookup name $ packedBranchs lookupCache
                                RefRemote name -> mapLookup name $ packedRemotes lookupCache
                                _              -> return Nothing
                  where mapLookup name m = maybe onFailure (return . Just) $ M.lookup name m

        namedResolvers = case prefix of
                             "HEAD"       -> [ RefHead ]
                             "ORIG_HEAD"  -> [ RefOrigHead ]
                             "FETCH_HEAD" -> [ RefFetchHead ]
                             _            -> map (flip ($) (RefName prefix)) [RefTag,RefBranch,RefRemote]

        tryResolvers :: [IO (Maybe Ref)] -> IO (Maybe Ref)
        tryResolvers []            = return $ if (isHexString prefix)
            then Just $ fromHexString prefix
            else Nothing
        tryResolvers (resolver:xs) = resolver >>= isResolved
           where isResolved (Just r) = return (Just r)
                 isResolved Nothing  = tryResolvers xs

        resolvePrePrefix :: IO (Maybe Ref)
        resolvePrePrefix
            | not (isHexString prefix) = return Nothing
            | otherwise = do
                refs <- findReferencesWithPrefix git prefix
                case refs of
                    []  -> return Nothing
                    [r] -> return (Just r)
                    _   -> error "multiple references with this prefix"

        modf [] ref                  = return (Just ref)
        modf (RevModParent i:xs) ref = do
            parentRefs <- getParentRefs ref
            case i of
                0 -> error "revision modifier ^0 is not implemented"
                _ -> case drop (i - 1) parentRefs of
                          []    -> error "no such parent"
                          (p:_) -> modf xs p

        modf (RevModParentFirstN 1:xs) ref = modf (RevModParent 1:xs) ref
        modf (RevModParentFirstN n:xs) ref = do
            parentRefs <- getParentRefs ref
            modf (RevModParentFirstN (n-1):xs) (head parentRefs)
        modf (_:_) _ = error "unimplemented revision modifier"

        getParentRefs ref = commitParents <$> getCommit git ref

-- | returns a tree from a ref that might be either a commit, a tree or a tag.
resolveTreeish :: Git -> Ref -> IO (Maybe Tree)
resolveTreeish git ref = getObject git ref True >>= mapJustM recToTree
  where recToTree (objectToCommit -> Just (Commit { commitTreeish = tree })) = resolveTreeish git tree
        recToTree (objectToTag    -> Just (Tag tref _ _ _ _))    = resolveTreeish git tref
        recToTree (objectToTree   -> Just t@(Tree _))            = return $ Just t
        recToTree _                                              = return Nothing


-- | Rewrite a set of commits from a revision and returns the new ref.
--
-- If during revision traversal (diving) there's a commit with zero or multiple
-- parents then the traversal will stop regardless of the amount of parent requested.
--
-- calling "rewrite f 2 (revisionOf d)" on the following tree:
--
--          a <-- b <-- c <-- d
--
-- result in the following tree after mapping with f:
--
--          a <-- f(b) <-- f(c) <-- f(d)
--
rewrite :: Git                   -- ^ Repository
        -> (Commit -> IO Commit) -- ^ Mapping function
        -> Revision              -- ^ revision to start from
        -> Int                   -- ^ the number of parents to map
        -> IO Ref                -- ^ return the new head REF
rewrite git mapCommit revision nbParent = do
    ref <- fromMaybe (error "revision cannot be found") <$> resolveRevision git revision
    resolveParents nbParent ref >>= process . reverse

  where resolveParents :: Int -> Ref -> IO [ (Ref, Commit) ]
        resolveParents 0 ref = (:[]) . (,) ref <$> getCommit git ref
        resolveParents n ref = do commit <- getCommit git ref
                                  case commitParents commit of
                                       [parentRef] -> liftM ((ref,commit) :) (resolveParents (n-1) parentRef)
                                       _           -> return [(ref,commit)]

        process [] = error "nothing to rewrite"
        process ((_,commit):next) =
                    mapCommit commit >>= looseWrite (gitRepoPath git) . toObject >>= flip rewriteOne next

        rewriteOne prevRef [] = return prevRef
        rewriteOne prevRef ((_,commit):next) = do
                    newCommit <- mapCommit $ commit { commitParents = [prevRef] }
                    ref       <- looseWrite (gitRepoPath git) (toObject newCommit)
                    rewriteOne ref next

-- | build a hierarchy tree from a tree object
buildHTree :: Git -> Tree -> IO HTree
buildHTree git (Tree ents) = mapM resolveTree ents
  where resolveTree (perm, ent, ref) = do
            obj <- getObjectType git ref
            case obj of
                Just TypeBlob -> return (perm, ent, TreeFile ref)
                Just TypeTree -> do ctree <- getTree git ref
                                    dir   <- buildHTree git ctree
                                    return (perm, ent, TreeDir ref dir)
                Just _        -> error "wrong type embedded in tree object"
                Nothing       -> error "unknown reference in tree object"

-- | resolve the ref (tree or blob) related to a path at a specific commit ref
resolvePath :: Git     -- ^ repository
            -> Ref     -- ^ commit reference
            -> EntPath -- ^ paths
            -> IO (Maybe Ref)
resolvePath git commitRef paths =
    getCommit git commitRef >>= \commit -> resolve (commitTreeish commit) paths
  where resolve :: Ref -> EntPath -> IO (Maybe Ref)
        resolve treeRef []     = return $ Just treeRef
        resolve treeRef (x:xs) = do
            (Tree ents) <- getTree git treeRef
            let cEnt = treeEntRef <$> findEnt x ents
            if xs == []
                then return cEnt
                else maybe (return Nothing) (\z -> resolve z xs) cEnt

        findEnt x = find (\(_, b, _) -> b == x)
        treeEntRef (_,_,r) = r

-- | Write a branch to point to a specific reference
branchWrite :: Git     -- ^ repository
            -> RefName -- ^ the name of the branch to write
            -> Ref     -- ^ the reference to set
            -> IO ()
branchWrite git branchName ref =
    writeRefFile (gitRepoPath git) (RefBranch branchName) (RefDirect ref)

-- | Return the list of branches
branchList :: Git -> IO (Set RefName)
branchList git = do
    ps <- Set.fromList . M.keys . packedBranchs <$> getCacheVal (packedNamed git)
    ls <- Set.fromList <$> looseHeadsList (gitRepoPath git)
    return $ Set.union ps ls

-- | Write a tag to point to a specific reference
tagWrite :: Git     -- ^ repository
         -> RefName -- ^ the name of the tag to write
         -> Ref     -- ^ the reference to set
         -> IO ()
tagWrite git tagname ref =
    writeRefFile (gitRepoPath git) (RefTag tagname) (RefDirect ref)

-- | Return the list of branches
tagList :: Git -> IO (Set RefName)
tagList git = do
    ps <- Set.fromList . M.keys . packedTags <$> getCacheVal (packedNamed git)
    ls <- Set.fromList <$> looseTagsList (gitRepoPath git)
    return $ Set.union ps ls

-- | Set head to point to either a reference or a branch name.
headSet :: Git                -- ^ repository
        -> Either Ref RefName -- ^ either a raw reference or a branch name
        -> IO ()
headSet git (Left ref)      =
    writeRefFile (gitRepoPath git) RefHead (RefDirect ref)
headSet git (Right refname) =
    writeRefFile (gitRepoPath git) RefHead (RefLink $ RefBranch refname)

-- | Get what the head is pointing to, or the reference otherwise
headGet :: Git
        -> IO (Either Ref RefName)
headGet git = do
    content <- readRefFile (gitRepoPath git) RefHead
    case content of
        RefLink (RefBranch b) -> return $ Right b
        RefLink spec          -> error ("unknown content link in HEAD: " ++ show spec)
        RefDirect r           -> return $ Left r
        RefContentUnknown bs  -> error ("unknown content in HEAD: " ++ show bs)

-- | Read the Config
configGetAll :: Git -> IO [Config]
configGetAll git = readIORef (configs git)

-- | Get a configuration element from the config file, starting from the
-- local repository config file, then the global config file.
--
-- for example the equivalent to git config user.name is:
--
-- > configGet git "user" "name"
--
configGet :: Git               -- ^ Git context
          -> String            -- ^ section name
          -> String            -- ^ key name
          -> IO (Maybe String) -- ^ The resulting value if it exists
configGet git section key = do
    cfgs <- configGetAll git
    return $ Cfg.get cfgs section key
