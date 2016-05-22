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
    , readFile
    , readTextFile
    , writeTextFile
    , readBinaryFile
    , readBinaryFileLazy
    , writeBinaryFile
    , hClose
    , IOMode(..)
    , Handle
    , createParentDirectory
    , createDirectory
    , writeFile
    , isFile
    , isDirectory
    , valid
    , getSize
    , MTime(..)
    , timeZero
    , getMTime
    , withFile
    , rename
    , removeFile
    , getEnvAsPath
    , absolute
    , parent
    , stripPrefix
    , localPathEncode
    , localPathDecode
    ) where

import           Data.Git.Imports
import           Filesystem.Path.CurrentOS
import           Filesystem hiding (readTextFile, writeTextFile)
import qualified Filesystem.Path.Rules as Rules
import           System.PosixCompat.Files (getFileStatus, modificationTime)
import           System.PosixCompat.Types (EpochTime)
import           System.IO (hClose)
import           System.Environment
import           Prelude hiding (FilePath, writeFile, readFile)
import qualified Prelude
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

type LocalPath = FilePath

listDirectoryFilename :: LocalPath -> IO [String]
listDirectoryFilename dir =
     map (Rules.encodeString Rules.posix . filename) <$> listDirectory dir

createParentDirectory :: LocalPath -> IO ()
createParentDirectory filepath = createTree $ parent filepath

readTextFile :: LocalPath -> IO String
readTextFile filepath = Prelude.readFile (encodeString filepath)

writeTextFile :: LocalPath -> String -> IO ()
writeTextFile filepath = Prelude.writeFile (encodeString filepath)

newtype MTime = MTime EpochTime deriving (Eq,Ord)

timeZero :: EpochTime
timeZero = 0

getMTime :: LocalPath -> IO MTime
getMTime filepath = MTime . modificationTime <$> getFileStatus (encodeString filepath)

getEnvAsPath :: String -> IO LocalPath
getEnvAsPath envName = Rules.decodeString Rules.posix <$> getEnv envName

localPathDecode :: B.ByteString -> LocalPath
localPathDecode = Rules.decode Rules.posix

localPathEncode :: LocalPath -> B.ByteString
localPathEncode = Rules.encode Rules.posix

readBinaryFile :: LocalPath -> IO B.ByteString
readBinaryFile lp = readFile lp

writeBinaryFile :: LocalPath -> B.ByteString -> IO ()
writeBinaryFile = writeFile

readBinaryFileLazy :: LocalPath -> IO L.ByteString
readBinaryFileLazy lp = L.readFile (encodeString lp)
