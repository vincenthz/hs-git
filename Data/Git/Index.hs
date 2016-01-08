module Data.Git.Index
    (
    ) where

import Control.Applicative
import Data.Git.Ref
{-
<INDEX_HEADER>
    :   "DIRC" <INDEX_FILE_VERSION> <INDEX_ENTRY_COUNT>
    ;
-}
data IndexHeader = IndexHeader Word32 Word32
    deriving (Show,Eq)

parseBe32 = be32 <$> A.take 4
parseBe16 = be16 <$> A.take 2
parseRef  = fromBinary <$> A.take 20

parseIndexHeader = do
        magic   <- parseBe32
        when (magic /= 'DIRC') $ error "wrong magic number for index"
        ver     <- parseBe32
        when (ver /= 2) $ error "unsupported packIndex version"
        entries <- parseBe32
        return $ IndexHeader ver entries

{-
<INDEX_FILE_FORMAT_V2>
    :   <INDEX_HEADER>
        <EXTENDED_INDEX_CONTENTS>
        <EXTENDED_CHECKSUM>
    ;

<EXTENDED_CHECKSUM>
    :   _sha-1_digest_( <EXTENDED_INDEX_CONTENTS> )
    ;

<INDEX_CHECKSUM>
    :   _sha-1_digest_( <INDEX_CONTENTS> )
    ;

-}

parseIndexContents entries = replicateM entries parseIndexEntry

{-
<EXTENDED_INDEX_CONTENTS>
    :   <INDEX_CONTENTS>
        <INDEX_CONTENTS_EXTENSIONS>
    ;

-}
parseIndexEntry = do
    -- INDEX_ENTRY_STAT_INFO
    ctime <- parseTime
    mtime <- parseTime
    dev   <- parseBe32
    inode <- parseBe32
    mode  <- parseBe32
    uid   <- parseBe32
    gid   <- parseBe32
    size  <- parseBe32
    -- entry id, flags, name, zero padding
   -- how to parse <ENTRY_ID>
    flags <- parseBe16
    -- 16 bit, network byte order, binary integer.
    --   bits 15-14  Reserved
    --   bits 13-12  Entry stage
    --   bits 11-0   Name byte length
    name  <- takeWhileNotNull
    zeroPadding

{-
<ENTRY_ZERO_PADDING>
    # The minimum length 0x00 byte sequence necessary to make the
    # written of digested byte length of the <INDEX_ENTRY> a
    # multiple of 8.
    ;
-}

parseTime = (,) <$> parseBe32 <*> parseBe32

{-
<ENTRY_ID>
    # Object ID of the of the file system entity contents.
    ;

<ENTRY_NAME>
    # File system entity name. Path is normalized and relative to
    # the working directory.
    ;

<INDEX_CONTENTS_EXTENSIONS>
    :   ( <INDEX_EXTENSION> )*
    ;
-}

parseIndexExtension = do
    -- # 4 byte sequence identifying how the <INDEX_EXTENSION_DATA>
    -- # should be interpreted. If the first byte has a value greater
    -- # than or equal to the ASCII character 'A' (0x41) and less than
    -- # or equal to the ASCII character 'Z' (0x5a), the extension is
    -- # optional and does not affect the interpretation of the other
    -- # contents in the index file. Any non-optional extensions must
    -- # be understood by the reading application to correctly
    -- # interpret the index file contents.
    name     <- A.take 4
    dataSize <- parseBe32
    data_    <- A.take dataSize
    return (name,data_)
