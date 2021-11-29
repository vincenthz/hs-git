-- |
-- Module      : Data.Git.OS
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unix
--
-- dealing with operating system / IO related stuff
-- like file on disk
module Data.Git.OS
    ( LocalPath
    -- * re-export
    , getHomeDirectory
    , getWorkingDirectory
    , (</>)
    , listDirectoryFilename
    , listDirectory
    , openFile
    , readTextFile
    , writeTextFile
    , readBinaryFile
    , readBinaryFileLazy
    , writeBinaryFile
    , hClose
    , IOMode(..)
    , Handle
    , createParentDirectory
    , createDirectoryIfMissing
    , doesFileExist
    , doesDirectoryExist
    , getFileSize
    , MTime(..)
    , timeZero
    , getMTime
    , withFile
    , renameFile
    , removeFile
    , getEnvAsPath
    , isAbsolute
    , takeDirectory
    , stripPrefix
    ) where

import           Data.Git.Imports
import           Data.List
import           System.Directory
import           System.FilePath.Posix
import           System.PosixCompat.Files (getFileStatus, modificationTime)
import           System.PosixCompat.Types (EpochTime)
import           System.IO
import           System.Environment
import           System.Posix.Directory (getWorkingDirectory)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

type LocalPath = FilePath

listDirectoryFilename :: LocalPath -> IO [FilePath]
listDirectoryFilename = listDirectory

createParentDirectory :: LocalPath -> IO ()
createParentDirectory = createDirectoryIfMissing True . takeDirectory

readTextFile :: LocalPath -> IO String
readTextFile = readFile

writeTextFile :: LocalPath -> String -> IO ()
writeTextFile = writeFile

newtype MTime = MTime EpochTime deriving (Eq,Ord)

timeZero :: EpochTime
timeZero = 0

getMTime :: LocalPath -> IO MTime
getMTime filepath = MTime . modificationTime <$> getFileStatus filepath

getEnvAsPath :: String -> IO LocalPath
getEnvAsPath envName = getEnv envName

readBinaryFile :: LocalPath -> IO B.ByteString
readBinaryFile = B.readFile

writeBinaryFile :: LocalPath -> B.ByteString -> IO ()
writeBinaryFile = B.writeFile

readBinaryFileLazy :: LocalPath -> IO L.ByteString
readBinaryFileLazy = L.readFile
