-- |
-- Module      : Data.Git.Internal
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unix
--
module Data.Git.Internal
    ( be32
    , be16
    , Zipped(..)
    , readZippedFile
    , dezip
    ) where

import Control.Applicative
import Data.Bits
import Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Codec.Compression.Zlib
import Filesystem.Path
import Filesystem.Path.Rules
import Prelude hiding (FilePath)

be32 :: B.ByteString -> Word32
be32 b = fromIntegral (B.index b 0) `shiftL` 24
       + fromIntegral (B.index b 1) `shiftL` 16
       + fromIntegral (B.index b 2) `shiftL` 8
       + fromIntegral (B.index b 3)

be16 :: B.ByteString -> Word16
be16 b = fromIntegral (B.index b 0) `shiftL` 8
       + fromIntegral (B.index b 1)

newtype Zipped = Zipped { getZippedData :: L.ByteString }
    deriving (Show,Eq)

readZippedFile :: FilePath -> IO Zipped
readZippedFile fp = Zipped <$> L.readFile (encodeString posix fp)

dezip :: Zipped -> L.ByteString
dezip = decompress . getZippedData
