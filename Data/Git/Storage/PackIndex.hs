-- |
-- Module      : Data.Git.Storage.PackIndex
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unix
--
{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Data.Git.Storage.PackIndex
    ( PackIndexHeader(..)
    , PackIndex(..)

    -- * handles and enumeration
    , packIndexOpen
    , packIndexClose
    , withPackIndex
    , packIndexEnumerate

    -- * read from packIndex
    , packIndexHeaderGetNbWithPrefix
    , packIndexGetReferenceLocation
    , packIndexGetReferencesWithPrefix
    , packIndexReadHeader
    , packIndexRead
    , packIndexGetHeader
    ) where

import Data.List
import Data.Bits
import Data.Word
import Data.ByteString (ByteString)

import Data.Vector (Vector, (!))
import qualified Data.Vector as V

import Data.Git.Internal
import Data.Git.Imports
import Data.Git.OS
import Data.Git.Storage.FileReader
import Data.Git.Path
import Data.Git.Ref
import qualified Data.Git.Parser as P

-- | represent an packIndex header with the version and the fanout table
data PackIndexHeader = PackIndexHeader !Word32 !(Vector Word32)
    deriving (Show,Eq)

data PackIndex hash = PackIndex
    { packIndexSha1s        :: Vector (Ref hash)
    , packIndexCRCs         :: Vector Word32
    , packIndexPackoffs     :: Vector Word32
    , packIndexPackChecksum :: Ref hash
    , packIndexChecksum     :: Ref hash
    }

-- | enumerate every indexes file in the pack directory
packIndexEnumerate :: HashAlgorithm hash => LocalPath -> IO [Ref hash]
packIndexEnumerate repoPath = map onlyHash . filter isPackFile <$> listDirectoryFilename (repoPath </> "objects" </> "pack")
  where
    isPackFile :: String -> Bool
    isPackFile x = ".idx" `isSuffixOf` x && "pack-" `isPrefixOf` x
    onlyHash = fromHexString . takebut 4 . drop 5
    takebut n l = take (length l - n) l

-- | open an index
packIndexOpen :: LocalPath -> Ref hash -> IO FileReader
packIndexOpen repoPath indexRef = openFile (indexPath repoPath indexRef) ReadMode >>= fileReaderNew False

-- | close an index
packIndexClose :: FileReader -> IO ()
packIndexClose = fileReaderClose

-- | variant of withFile on the index file and with a FileReader
withPackIndex :: LocalPath -> Ref hash -> (FileReader -> IO a) -> IO a
withPackIndex repoPath indexRef = withFileReader (indexPath repoPath indexRef)

-- | returns the number of references, referenced in this index.
packIndexHeaderGetSize :: PackIndexHeader -> Word32
packIndexHeaderGetSize (PackIndexHeader _ indexes) = indexes ! 255

-- | byte size of an packIndex header.
packIndexHeaderByteSize :: Int
packIndexHeaderByteSize = 2*4 {- header -} + 256*4 {- fanout table -}

-- | get the number of reference in this index with a specific prefix
packIndexHeaderGetNbWithPrefix :: PackIndexHeader -> Int -> Word32
packIndexHeaderGetNbWithPrefix (PackIndexHeader _ indexes) n
        | n < 0 || n > 255 = 0
        | n == 0           = indexes ! 0
        | otherwise        = (indexes ! n) - (indexes ! (n-1))

-- | fold on refs with a specific prefix
packIndexHeaderFoldRef :: HashAlgorithm hash
                       => PackIndexHeader
                       -> FileReader
                       -> hash
                       -> Int
                       -> (a -> Word32 -> Ref hash -> (a, Bool))
                       -> a
                       -> IO a
packIndexHeaderFoldRef idxHdr@(PackIndexHeader _ indexes) fr alg refprefix f initAcc
    | nb == 0   = return initAcc
    | otherwise = do
        let spos     = (indexes ! refprefix) - nb
            hashSize = hashDigestSize alg
        fileReaderSeek fr (fromIntegral (sha1Offset + spos * fromIntegral hashSize))
        loop nb initAcc
  where
    loop 0 acc = return acc
    loop n acc = do
        b <- fileReaderGetRef alg fr
        let (!nacc, terminate) = f acc (nb-n) b
        if terminate
            then return nacc
            else loop (n-1) nacc
    nb         = packIndexHeaderGetNbWithPrefix idxHdr refprefix
    (sha1Offset,_,_) = packIndexOffsets alg idxHdr

-- | return the reference offset in the packfile if found
packIndexGetReferenceLocation :: HashAlgorithm hash => PackIndexHeader -> FileReader -> Ref hash -> IO (Maybe Word64)
packIndexGetReferenceLocation idxHdr@(PackIndexHeader _ indexes) fr ref = do
    mrpos <- packIndexHeaderFoldRef idxHdr fr (hashAlgFromRef ref) refprefix f Nothing
    case mrpos of
        Nothing   -> return Nothing
        Just rpos -> do
            let spos = (indexes ! refprefix) - nb
            fileReaderSeek fr (fromIntegral (packOffset + 4 * (spos+rpos)))
            Just . fromIntegral . be32 <$> fileReaderGetBS 4 fr
  where
    f acc rpos rref = if ref == rref then (Just rpos,True) else (acc,False)
    refprefix  = refPrefix ref
    nb         = packIndexHeaderGetNbWithPrefix idxHdr refprefix
    (_,_,packOffset) = packIndexOffsets (hashAlgFromRef ref) idxHdr

-- | get all references that start by prefix.
packIndexGetReferencesWithPrefix :: HashAlgorithm hash => PackIndexHeader -> FileReader -> String -> IO [Ref hash]
packIndexGetReferencesWithPrefix idxHdr fr prefix =
    packIndexHeaderFoldRef idxHdr fr hashAlg refprefix f []
  where
    f acc _ ref = case cmpPrefix prefix ref of
        GT -> (acc    ,False)
        EQ -> (ref:acc,False)
        LT -> (acc    ,True)
    refprefix   = read ("0x" ++ take 2 prefix)

-- | returns absolute offset in the index file of the sha1s, the crcs and the packfiles offset.
packIndexOffsets :: HashAlgorithm hash => hash -> PackIndexHeader -> (Word32, Word32, Word32)
packIndexOffsets alg idx = (packIndexSha1sOffset, packIndexCRCsOffset, packIndexPackOffOffset)
  where
    packIndexPackOffOffset = packIndexCRCsOffset + crcsTableSz
    packIndexCRCsOffset    = packIndexSha1sOffset + sha1TableSz
    packIndexSha1sOffset   = fromIntegral packIndexHeaderByteSize
    crcsTableSz        = 4 * sz
    sha1TableSz        = (fromIntegral $ hashDigestSize alg) * sz
    sz                 = packIndexHeaderGetSize idx

-- | parse index header
parsePackIndexHeader :: P.Parser PackIndexHeader
parsePackIndexHeader = do
    magic   <- P.word32
    when (magic /= 0xff744f63) $ error "wrong magic number for packIndex"
    ver     <- P.word32
    when (ver /= 2) $ error "unsupported packIndex version"
    fanouts <- V.replicateM 256 P.word32
    return $ PackIndexHeader ver fanouts

-- | read index header from an index filereader
packIndexReadHeader :: FileReader -> IO PackIndexHeader
packIndexReadHeader fr = fileReaderSeek fr 0 >> fileReaderParse fr parsePackIndexHeader

-- | get index header from an index reference
packIndexGetHeader :: LocalPath -> Ref hash -> IO PackIndexHeader
packIndexGetHeader repoPath indexRef = withPackIndex repoPath indexRef $ packIndexReadHeader

-- | read all index
packIndexRead :: HashAlgorithm hash
              => LocalPath
              -> Ref hash
              -> IO (PackIndexHeader, (Vector (Ref hash), Vector Word32, Vector Word32, [ByteString], Ref hash, Ref hash))
packIndexRead repoPath indexRef = do
    withPackIndex repoPath indexRef $ \fr -> do
        idx <- fileReaderParse fr parsePackIndexHeader
        liftM2 (,) (return idx) (fileReaderParse fr (parsePackIndex $ packIndexHeaderGetSize idx))
  where
    parsePackIndex sz = do
        sha1s     <- V.replicateM (fromIntegral sz) P.referenceBin
        crcs      <- V.replicateM (fromIntegral sz) P.word32
        packoffs  <- V.replicateM (fromIntegral sz) P.word32
        let nbLarge = length $ filter (== True) $ map (\packoff -> packoff `testBit` 31) $ V.toList packoffs
        largeoffs <- replicateM nbLarge (P.take 4)
        packfileChecksum <- P.referenceBin
        idxfileChecksum  <- P.referenceBin
        -- large packfile offsets
        -- trailer
        return (sha1s, crcs, packoffs, largeoffs, packfileChecksum, idxfileChecksum)
