-- |
-- Module      : Data.Git.Path
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unix
--
{-# LANGUAGE OverloadedStrings #-}
module Data.Git.Path where

import Filesystem.Path.CurrentOS
import System.Random
import Data.Git.Ref
import Data.Git.Imports
import Data.String

configPath gitRepo = gitRepo </> "config"

headsPath gitRepo = gitRepo </> "refs" </> "heads" </> ""
tagsPath gitRepo  = gitRepo </> "refs" </> "tags" </> ""
remotesPath gitRepo = gitRepo </> "refs" </> "remotes" </> ""
packedRefsPath gitRepo = gitRepo </> "packed-refs"

headPath gitRepo name = headsPath gitRepo </> fromString name
tagPath gitRepo name = tagsPath gitRepo </> fromString name
remotePath gitRepo name = remotesPath gitRepo </> fromString name
specialPath gitRepo name = gitRepo </> fromString name

remoteEntPath gitRepo name ent = remotePath gitRepo name </> fromString ent

packDirPath repoPath = repoPath </> "objects" </> "pack"

indexPath repoPath indexRef =
        packDirPath repoPath </> fromString ("pack-" ++ toHexString indexRef ++ ".idx")

packPath repoPath packRef =
        packDirPath repoPath </> fromString ("pack-" ++ toHexString packRef ++ ".pack")

objectPath repoPath d f = repoPath </> "objects" </> fromString d </> fromString f
objectPathOfRef repoPath ref = objectPath repoPath d f
        where (d,f) = toFilePathParts ref

objectTemporaryPath repoPath = do
        r <- fst . random <$> getStdGen :: IO Int
        return (repoPath </> "objects" </> fromString ("tmp-" ++ show r ++ ".tmp"))
