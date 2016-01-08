-- |
-- Module      : Data.Git.Ref
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unix
--
{-# LANGUAGE DeriveDataTypeable #-}
module Data.Git.Ref
    ( Ref
    -- * Exceptions
    , RefInvalid(..)
    , RefNotFound(..)
    -- * convert from bytestring and string
    , isHex
    , isHexString
    , fromHex
    , fromHexString
    , fromBinary
    , toBinary
    , toHex
    , toHexString
    -- * Misc function related to ref
    , refPrefix
    , cmpPrefix
    , toFilePathParts
    -- * Hash ByteString types to a ref
    , hash
    , hashLBS
    ) where

import Control.Monad (forM_)
import qualified Crypto.Hash.SHA1 as SHA1
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B (unsafeCreate)
import qualified Data.ByteString.Unsafe as B (unsafeIndex)
import qualified Data.ByteString.Char8 as BC
import Data.Bits
import Data.Char (isHexDigit)
import Data.Data

import Foreign.Storable
import Control.Exception (Exception, throw)

-- | represent a git reference (SHA1)
newtype Ref = Ref ByteString
    deriving (Eq,Ord,Data,Typeable)

instance Show Ref where
    show = BC.unpack . toHex

-- | Invalid Reference exception raised when
-- using something that is not a ref as a ref.
data RefInvalid = RefInvalid ByteString
    deriving (Show,Eq,Data,Typeable)

-- | Reference wasn't found
data RefNotFound = RefNotFound Ref
    deriving (Show,Eq,Data,Typeable)

instance Exception RefInvalid
instance Exception RefNotFound

isHex = and . map isHexDigit . BC.unpack
isHexString = and . map isHexDigit

-- | take a hexadecimal bytestring that represent a reference
-- and turn into a ref
fromHex :: ByteString -> Ref
fromHex s
    | B.length s == 40 = Ref $ B.unsafeCreate 20 populateRef
    | otherwise        = throw $ RefInvalid s
  where populateRef ptr = forM_ [0..19] $ \i -> do
            let v = (unhex (B.unsafeIndex s (i*2+0)) `shiftL` 4) .|. unhex (B.unsafeIndex s (i*2+1))
            pokeElemOff ptr (i+0) v

        unhex 0x30 = 0  -- '0'
        unhex 0x31 = 1
        unhex 0x32 = 2
        unhex 0x33 = 3
        unhex 0x34 = 4
        unhex 0x35 = 5
        unhex 0x36 = 6
        unhex 0x37 = 7
        unhex 0x38 = 8
        unhex 0x39 = 9  -- '9'
        unhex 0x41 = 10 -- 'A'
        unhex 0x42 = 11
        unhex 0x43 = 12
        unhex 0x44 = 13
        unhex 0x45 = 14
        unhex 0x46 = 15 -- 'F'
        unhex 0x61 = 10 -- 'a'
        unhex 0x62 = 11
        unhex 0x63 = 12
        unhex 0x64 = 13
        unhex 0x65 = 14
        unhex 0x66 = 15 -- 'f'
        unhex _    = throw $ RefInvalid s

-- | take a hexadecimal string that represent a reference
-- and turn into a ref
fromHexString :: String -> Ref
fromHexString = fromHex . BC.pack

-- | transform a ref into an hexadecimal bytestring
toHex :: Ref -> ByteString
toHex (Ref bs) = B.unsafeCreate 40 populateHex
  where populateHex ptr = forM_ [0..19] $ \i -> do
            let (a,b) = B.unsafeIndex bs i `divMod` 16
            pokeElemOff ptr (i*2+0) (hex a)
            pokeElemOff ptr (i*2+1) (hex b)
        hex i
            | i >= 0 && i <= 9   = 0x30 + i
            | i >= 10 && i <= 15 = 0x61 + i - 10
            | otherwise          = 0

-- | transform a ref into an hexadecimal string
toHexString :: Ref -> String
toHexString = BC.unpack . toHex

-- | transform a bytestring that represent a binary bytestring
-- and returns a ref.
fromBinary :: ByteString -> Ref
fromBinary b
    | B.length b == 20 = Ref b
    | otherwise        = throw $ RefInvalid b -- should hexify the ref here

-- | turn a reference into a binary bytestring
toBinary :: Ref -> ByteString
toBinary (Ref b) = b

-- | returns the prefix (leading byte) of this reference
refPrefix :: Ref -> Int
refPrefix (Ref b) = fromIntegral $ B.unsafeIndex b 0

-- | compare prefix
cmpPrefix :: String -> Ref -> Ordering
cmpPrefix pre ref = pre `compare` (take (length pre) $ toHexString ref)

-- | returns the splitted format "prefix/suffix" for addressing the loose object database
toFilePathParts :: Ref -> (String, String)
toFilePathParts ref = splitAt 2 $ show ref

-- | hash a bytestring into a reference
hash = Ref . SHA1.hash

-- | hash a lazy bytestring into a reference
hashLBS = Ref . SHA1.hashlazy
