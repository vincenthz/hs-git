{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |
-- Module      : Data.Git.WorkTree
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unix
--
-- a load-on-demand, write-on-demand working tree.
--
module Data.Git.WorkTree
    ( WorkTree
    , EntType(..)
    -- * Create new work trees
    , workTreeNew
    , workTreeFrom
    -- * Modifications methods
    , workTreeDelete
    , workTreeSet
    , workTreeFlush
    ) where

import Data.Git.Ref
import Data.Git.Types
import Data.Git.Storage.Object
import Data.Git.Storage
import Data.Git.Repository

--import qualified Data.ByteString as B

import qualified Data.Map as M

import Control.Monad
import Control.Concurrent.MVar

type Dir = M.Map EntName (ModePerm, TreeSt)
type TreeVar = MVar Dir
data TreeSt = TreeRef Ref | TreeLoaded TreeVar
type WorkTree = MVar TreeSt

data EntType = EntDirectory | EntFile | EntExecutable
    deriving (Show,Eq)

-- | Create a new worktree
workTreeNew :: IO WorkTree
workTreeNew = newMVar M.empty >>= newMVar . TreeLoaded

-- | Create a worktree from a tree reference.
workTreeFrom :: Ref -> IO WorkTree
workTreeFrom ref = newMVar (TreeRef ref)

-- | delete a path from a working tree
--
-- if the path doesn't exist, no error is raised
workTreeDelete :: Git -> WorkTree -> EntPath -> IO ()
workTreeDelete git wt path = diveFromRoot git wt path dive
  where dive _          []     = error "internal error: delete: empty dive"
        dive varCurrent [file] = modifyMVar_ varCurrent (return . M.delete file)
        dive varCurrent (x:xs) = do
            evarChild <- loadOrGetTree git x varCurrent $ \m -> return (m, Right ())
            case evarChild of
                Left varChild -> dive varChild xs
                Right ()      -> return ()

-- | Set a file in this working tree to a specific ref.
--
-- The ref should point to a valid blob or tree object, and
-- it's safer to write the referenced tree or blob object first.
workTreeSet :: Git -> WorkTree -> EntPath -> (EntType, Ref) -> IO ()
workTreeSet git wt path (entType, entRef) = diveFromRoot git wt path dive
  where dive :: TreeVar -> EntPath -> IO ()
        dive _          []     = error "internal error: set: empty dive"
        dive varCurrent [file] = modifyMVar_ varCurrent (return . M.insert file (entTypeToPerm entType, TreeRef entRef))
        dive varCurrent (x:xs) = do
            evarChild <- loadOrGetTree git x varCurrent $ \m -> do
                            -- create an empty tree
                            v <- newMVar M.empty
                            return (M.insert x (entTypeToPerm EntDirectory, TreeLoaded v) m, Left v)
            case evarChild of
                Left varChild -> dive varChild xs
                Right ()      -> return ()

{-
workTreeFlushAt :: Git -> WorkTree -> EntPath -> IO ()
workTreeFlushAt git wt path = do
    undefined
-}

-- | Flush the worktree by creating all the necessary trees in the git store
-- and return the root ref of the work tree.
workTreeFlush :: Git -> WorkTree -> IO Ref
workTreeFlush git wt = do
    -- write all the trees that need to be written
    -- switch to modifyMVar
    wtVal <- takeMVar wt
    case wtVal of
        TreeRef ref    -> putMVar wt wtVal >> return ref
        TreeLoaded var -> do
            ref <- writeTreeRecursively (TreeLoaded var)
            putMVar wt $ TreeRef ref
            return ref
  where writeTreeRecursively (TreeRef ref) = return ref
        writeTreeRecursively (TreeLoaded var) = do
            c <- readMVar var
            ents <- forM (M.toList c) $ \(bs, (mperm, entSt)) -> do
                        ref <- writeTreeRecursively entSt
                        return (mperm, bs, ref)
            setTree ents

        setTree ents = setObject git (toObject $ Tree ents)

----- helpers -----

loadTreeVar :: Git -> Ref -> IO TreeVar
loadTreeVar git treeRef = do
    (Tree ents) <- getTree git treeRef
    let t = foldr (\(m,b,r) acc -> M.insert b (m,TreeRef r) acc) M.empty ents
    newMVar t

entTypeToPerm :: EntType -> ModePerm
entTypeToPerm EntDirectory  = ModePerm 0o040000
entTypeToPerm EntExecutable = ModePerm 0o100755 
entTypeToPerm EntFile       = ModePerm 0o100644

loadOrGetTree :: Git -> EntName -> TreeVar -> (Dir -> IO (Dir, Either TreeVar a)) -> IO (Either TreeVar a)
loadOrGetTree git x varCurrent onMissing =
    modifyMVar varCurrent $ \m -> do
        case M.lookup x m of
            Nothing          -> onMissing m
            Just (_, treeSt) -> -- check perm to see if it is a directory
                case treeSt of
                    TreeRef ref -> do
                        -- replace the ref by a loaded tree
                        var <- loadTreeVar git ref
                        return (M.adjust (\(perm,_) -> (perm, TreeLoaded var)) x m, Left var)
                    TreeLoaded var -> return (m, Left var)

diveFromRoot :: Git -> WorkTree -> EntPath
             -> (TreeVar -> EntPath -> IO ())
             -> IO () 
diveFromRoot git wt path dive
    | path == [] = return ()
    | otherwise    = do
        -- switch to modifyMVar
        wtVal   <- takeMVar wt
        current <- case wtVal of
                    TreeLoaded var -> return var
                    TreeRef ref      -> loadTreeVar git ref
        putMVar wt $ TreeLoaded current
        dive current path

