-- |
-- Module      : Data.Git.Storage.FileWriter
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unix
--
module Data.Git.Storage.FileWriter where

import Data.Git.Ref
import Data.Git.OS
import Data.IORef
import qualified Data.ByteString as B
import Codec.Zlib
import Control.Exception (bracket)

import Crypto.Hash

defaultCompression :: Int
defaultCompression = 6

-- this is a copy of modifyIORef' found in base 4.6 (ghc 7.6),
-- for older version of base.
modifyIORefStrict :: IORef a -> (a -> a) -> IO ()
modifyIORefStrict ref f = do
    x <- readIORef ref
    let x' = f x
    x' `seq` writeIORef ref x'

data FileWriter hash = FileWriter
        { writerHandle  :: Handle
        , writerDeflate :: Deflate
        , writerDigest  :: IORef (Context hash)
        }

fileWriterNew :: HashAlgorithm hash => Handle -> IO (FileWriter hash)
fileWriterNew handle = do
        deflate <- initDeflate defaultCompression defaultWindowBits
        digest  <- newIORef hashInit
        return $ FileWriter
                { writerHandle  = handle
                , writerDeflate = deflate
                , writerDigest  = digest
                }

withFileWriter :: HashAlgorithm hash => LocalPath -> (FileWriter hash -> IO c) -> IO c
withFileWriter path f =
        bracket (openFile path WriteMode) (hClose) $ \handle ->
                bracket (fileWriterNew handle) (fileWriterClose) f

postDeflate :: Handle -> Maybe B.ByteString -> IO ()
postDeflate handle = maybe (return ()) (B.hPut handle)

fileWriterOutput :: HashAlgorithm hash => FileWriter hash -> B.ByteString -> IO ()
fileWriterOutput (FileWriter { writerHandle = handle, writerDigest = digest, writerDeflate = deflate }) bs = do
        modifyIORefStrict digest (\ctx -> hashUpdate ctx bs)
        (>>= postDeflate handle) =<< feedDeflate deflate bs

fileWriterClose :: FileWriter hash -> IO ()
fileWriterClose (FileWriter { writerHandle = handle, writerDeflate = deflate }) =
        postDeflate handle =<< finishDeflate deflate

fileWriterGetDigest :: HashAlgorithm hash => FileWriter hash -> IO (Ref hash)
fileWriterGetDigest (FileWriter { writerDigest = digest }) = (fromDigest . hashFinalize) `fmap` readIORef digest
