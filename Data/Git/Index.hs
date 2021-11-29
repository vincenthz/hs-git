{-# OPTIONS_GHC -fwarn-missing-signatures -fno-warn-unused-binds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Git.Index
    ( IndexEntry( .. )
    , parseIndex
    , decodeIndex
    , loadIndexFile
    , indexEntryOfFile
    ) where

import Control.Monad( when
                    , replicateM
                    )
import Data.Bits
    ( unsafeShiftL
    , unsafeShiftR
    , testBit
    , (.&.)
    , (.|.)
    )
import Data.Word( Word8, Word16, Word32 )
import Data.Git.Ref
import qualified Control.Exception as E
import qualified Data.Vector as V
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Git.Parser as P
import           Data.Git.Parser (Parser)

data IndexHeader = IndexHeader 
    { indexFileVersion :: {-# UNPACK #-} !Word32
    , indexEntryCount  :: {-# UNPACK #-} !Word32
    }
    deriving (Show, Eq)

-- | Finds an index in a given vector, which must be sorted with respect to the
-- given comparison function, at which the given element could be inserted while
-- preserving the vector's sortedness.
binarySearchBy :: (e -> a -> Ordering) -> V.Vector e -> a -> Maybe e
{-# INLINE binarySearchBy #-}
binarySearchBy cmp vec e = go 0 (length vec)
  where
    go !l !u | u <= l    = Nothing
    go !l !u =
        let !k = (u + l) `unsafeShiftR` 1
            !e' = V.unsafeIndex vec k in
        case cmp e' e of
            LT -> go (k+1) u
            EQ -> Just e'
            GT -> go l k

indexEntryOfFile :: HashAlgorithm hash => B.ByteString -> V.Vector (IndexEntry hash) -> Maybe (IndexEntry hash)
indexEntryOfFile path vec = binarySearchBy (compare . fileName) vec path

loadIndexFile :: HashAlgorithm hash => FilePath -> IO (Either String (V.Vector (IndexEntry hash)))
loadIndexFile path = (decodeIndex <$> BC.readFile path) `E.catch` onError
  where
    onError :: E.SomeException -> IO (Either String a)
    onError _ = return $ Left "Cannot find index file"

decodeIndex :: HashAlgorithm hash => B.ByteString -> Either String (V.Vector (IndexEntry hash))
decodeIndex = P.eitherParse parseIndex

parseIndex :: HashAlgorithm hash => Parser (V.Vector (IndexEntry hash))
parseIndex = do
    hdr <- parseIndexHeader
    V.replicateM (fromIntegral $ indexEntryCount hdr) parseIndexEntry

parseIndexHeader :: Parser IndexHeader
parseIndexHeader = do
    magic <- P.take 4
    when (magic /= "DIRC") $ fail "wrong magic number for index"
    ver <- P.word32
    when (ver `notElem` [2, 3]) $ fail "unsupported packIndex version"
    entries <- P.word32
    return $ IndexHeader ver entries

-- Index entries are sorted in ascending order on the name field,
-- interpreted as a string of unsigned bytes (i.e. memcmp() order, no
-- localization, no special casing of directory separator '/'). Entries
-- with the same name are sorted by their stage field.
data IndexEntry hash = IndexEntry
    { -- | 32-bit ctime seconds, the last time a file's metadata changed
      ctime :: !Word32
    -- | 32-bit ctime nanosecond fractions
    , ctimeNano :: !Word32
    -- | 32-bit mtime seconds, the last time a file's data changed
    , mtime :: !Word32
    -- | 32-bit mtime nanosecond fractions
    , mtimeNano :: !Word32
    -- | 32-bit dev
    , dev :: !Word32
    -- | 32-bit ino
    , ino :: !Word32
    -- | 32-bit mode, split into (high to low bits)
    -- 
    -- 4-bit object type
    --   valid values in binary are 1000 (regular file), 1010 (symbolic link)
    --   and 1110 (gitlink)
    -- 
    -- 3-bit unused
    -- 
    -- 9-bit unix permission. Only 0755 and 0644 are valid for regular files.
    -- Symbolic links and gitlinks have value 0 in this field.
    , mode :: !Word32
    -- | 32-bit uid
    , uid  :: !Word32
    -- | 32-bit gid
    , gid  :: !Word32
    -- | 32-bit file size This is the on-disk size from stat(2), truncated to 32-bit.
    , fileSize :: !Word32
    -- | 160-bit SHA-1 for the represented object
    , fileHash :: !(Ref hash)

    -- | A 16-bit 'flags' field
    , flags :: !IndexEntryFlags
    -- (Version 3 or later) A 16-bit field, only applicable if the
    -- "extended flag" above is 1, split into (high to low bits).
    -- 
    -- 1-bit reserved for future
    -- 
    -- 1-bit skip-worktree flag (used by sparse checkout)
    -- 
    -- 1-bit intent-to-add flag (used by "git add -N")
    -- 
    -- 13-bit unused, must be zero
    , extended :: !Word16

    , fileName :: !B.ByteString
    }
    deriving (Eq, Show)

data IndexEntryFlags = IndexEntryFlags 
    { -- | 1-bit assume-valid flag
      iefAssumeValid :: !Bool
      -- | 1-bit extended flag (must be zero in version 2)
    , iefExtended    :: !Bool
      -- | 2-bit stage (during merge)
    , iefStage       :: {-# UNPACK #-} !Word8
      -- | 12-bit name length if the length is less than 0xFFF;
      -- otherwise 0xFFF is stored in this field.
    , iefNameLength  :: {-# UNPACK #-} !Word16
    }
    deriving (Eq, Show)

flagsOfWord :: Word16 -> IndexEntryFlags
flagsOfWord w = IndexEntryFlags
    { iefAssumeValid = w `testBit` 15
    , iefExtended    = w `testBit` 14
    , iefStage       = fromIntegral $ (w `unsafeShiftR` 13) .&. 0x3
    , iefNameLength  = w .&. 0xFFF
    }

parseIndexEntry :: HashAlgorithm hash => Parser (IndexEntry hash)
parseIndexEntry = do
    vCtime <- P.word32      -- +4   4
    vCtimeNano <- P.word32  -- +4   8
    vMtime <- P.word32      -- +4   12
    vMtimeNano <- P.word32  -- +4   16
    vDev   <- P.word32      -- +4   20
    vInode <- P.word32 -- +4   24
    vMode  <- P.word32 -- +4   28
    vUid   <- P.word32 -- +4   32
    vGid   <- P.word32 -- +4   36
    vSize  <- P.word32 -- +4   40
    -- TODO ref is variable size now
    vFileHash  <- P.ref -- +20   60
    vFlags <- flagsOfWord <$> P.word16 -- +2 62
    vExtended <- if iefExtended vFlags -- +2 64
        then P.word16
        else return 0
    vName  <- P.take . fromIntegral $ iefNameLength vFlags
    let padding = 8 - ((62 + iefNameLength vFlags) `mod` 8)
    _ <- replicateM (fromIntegral padding) P.anyByte
    return IndexEntry
        { ctime = vCtime
        , ctimeNano = vCtimeNano
        , mtime = vMtime
        , mtimeNano = vMtimeNano
        , dev = vDev
        , ino = vInode
        , mode = vMode
        , uid  = vUid
        , gid  = vGid
        , fileSize = vSize
        , fileHash = vFileHash
        , flags = vFlags
        , extended = vExtended
        , fileName = vName
        }


{-
<INDEX_CONTENTS_EXTENSIONS>
    :   ( <INDEX_EXTENSION> )*
    ;
-}

parseIndexExtension :: P.Parser (BC.ByteString, BC.ByteString)
parseIndexExtension = do
    -- # 4 byte sequence identifying how the <INDEX_EXTENSION_DATA>
    -- # should be interpreted. If the first byte has a value greater
    -- # than or equal to the ASCII character 'A' (0x41) and less than
    -- # or equal to the ASCII character 'Z' (0x5a), the extension is
    -- # optional and does not affect the interpretation of the other
    -- # contents in the index file. Any non-optional extensions must
    -- # be understood by the reading application to correctly
    -- # interpret the index file contents.
    name     <- P.take 4
    dataSize <- P.word32
    data_    <- P.take $ fromIntegral dataSize
    return (name, data_)
