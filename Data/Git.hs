-- |
-- Module      : Data.Git
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unix
--
module Data.Git
    (
    -- * Basic types
      Ref
    , RefName(..)
    , Commit(..)
    , Person(..)
    , CommitExtra(..)
    , Tree(..)
    , Blob(..)
    , Tag(..)
    , GitTime
    , ModePerm(..)
    , EntName
    , EntPath
    , entName
    , entPathAppend

    -- * Helper & type related to ModePerm
    , ObjectFileType(..)
    , FilePermissions(..)
    , getPermission
    , getFiletype

    -- * Revision
    , Revision
    , resolveRevision

    -- * Object resolution
    , resolveTreeish
    , resolvePath

    -- * repo context
    , Git
    , withCurrentRepo
    , withRepo
    , findRepo

    -- * Repository queries and creation
    , initRepo
    , isRepo

    -- * Context operations
    , rewrite

    -- * Get objects
    , getObject
    , getCommit
    , getTree

    -- * Set objects
    , setObject
    , toObject

    -- * Work trees
    , WorkTree
    , EntType(..)
    , workTreeNew
    , workTreeFrom
    , workTreeDelete
    , workTreeSet
    , workTreeFlush

    -- * Named refs
    , branchWrite
    , branchList
    , tagWrite
    , tagList
    , headSet
    , headGet
    ) where

import Data.Git.Ref
import Data.Git.Types
import Data.Git.Storage
import Data.Git.Repository
import Data.Git.Revision
import Data.Git.Storage.Object (toObject)
import Data.Git.WorkTree
