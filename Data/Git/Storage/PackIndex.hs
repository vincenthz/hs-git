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

import Filesystem
import Filesystem.Path
import qualified Filesystem.Path.Rules as Rules

import Data.List
import Data.Bits
import Data.Word
import Data.String

import Data.Vector (Vector, (!))
import qualified Data.Vector as V

import Data.Git.Internal
import Data.Git.Imports
import Data.Git.Storage.FileReader
import Data.Git.Path
import Data.Git.Ref
import qualified Data.Git.Parser as P

import Prelude hiding (FilePath)

-- | represent an packIndex header with the version and the fanout table
data PackIndexHeader = PackIndexHeader !Word32 !(Vector Word32)
        deriving (Show,Eq)

data PackIndex = PackIndex
        { packIndexSha1s        :: Vector Ref
        , packIndexCRCs         :: Vector Word32
        , packIndexPackoffs     :: Vector Word32
        , packIndexPackChecksum :: Ref
        , packIndexChecksum     :: Ref
        }

-- | enumerate every indexes file in the pack directory
packIndexEnumerate repoPath = map onlyHash . filter isPackFile . map (Rules.encodeString Rules.posix . filename) <$> listDirectory (repoPath </> "objects" </> "pack")
  where
        isPackFile :: String -> Bool
        isPackFile x = ".idx" `isSuffixOf` x && "pack-" `isPrefixOf` x
        onlyHash = fromHexString . takebut 4 . drop 5
        takebut n l = take (length l - n) l

-- | open an index
packIndexOpen :: FilePath -> Ref -> IO FileReader
packIndexOpen repoPath indexRef = openFile (indexPath repoPath indexRef) ReadMode >>= fileReaderNew False

-- | close an index
packIndexClose :: FileReader -> IO ()
packIndexClose = fileReaderClose

-- | variant of withFile on the index file and with a FileReader
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
packIndexHeaderFoldRef :: PackIndexHeader -> FileReader -> Int -> (a -> Word32 -> Ref -> (a, Bool)) -> a -> IO a
packIndexHeaderFoldRef idxHdr@(PackIndexHeader _ indexes) fr refprefix f initAcc
        | nb == 0   = return initAcc
        | otherwise = do
                let spos = (indexes ! refprefix) - nb
                fileReaderSeek fr (fromIntegral (sha1Offset + spos * 20))
                loop nb initAcc
        where
                loop 0 acc = return acc
                loop n acc = do
                        b <- fromBinary <$> fileReaderGetBS 20 fr
                        let (!nacc, terminate) = f acc (nb-n) b
                        if terminate
                                then return nacc
                                else loop (n-1) nacc
                nb         = packIndexHeaderGetNbWithPrefix idxHdr refprefix
                (sha1Offset,_,_) = packIndexOffsets idxHdr

-- | return the reference offset in the packfile if found
packIndexGetReferenceLocation :: PackIndexHeader -> FileReader -> Ref -> IO (Maybe Word64)
packIndexGetReferenceLocation idxHdr@(PackIndexHeader _ indexes) fr ref = do
        mrpos <- packIndexHeaderFoldRef idxHdr fr refprefix f Nothing
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
                (_,_,packOffset) = packIndexOffsets idxHdr

-- | get all references that start by prefix.
packIndexGetReferencesWithPrefix :: PackIndexHeader -> FileReader -> String -> IO [Ref]
packIndexGetReferencesWithPrefix idxHdr fr prefix =
        packIndexHeaderFoldRef idxHdr fr refprefix f []
        where
                f acc _ ref = case cmpPrefix prefix ref of
                        GT -> (acc    ,False)
                        EQ -> (ref:acc,False)
                        LT -> (acc    ,True)
                refprefix   = read ("0x" ++ take 2 prefix)

-- | returns absolute offset in the index file of the sha1s, the crcs and the packfiles offset.
packIndexOffsets idx = (packIndexSha1sOffset, packIndexCRCsOffset, packIndexPackOffOffset)
        where
                packIndexPackOffOffset = packIndexCRCsOffset + crcsTableSz
                packIndexCRCsOffset    = packIndexSha1sOffset + sha1TableSz
                packIndexSha1sOffset   = fromIntegral packIndexHeaderByteSize
                crcsTableSz        = 4 * sz
                sha1TableSz        = 20 * sz
                sz                 = packIndexHeaderGetSize idx

-- | parse index header
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
packIndexGetHeader :: FilePath -> Ref -> IO PackIndexHeader
packIndexGetHeader repoPath indexRef = withPackIndex repoPath indexRef $ packIndexReadHeader

-- | read all index
packIndexRead repoPath indexRef = do
        withPackIndex repoPath indexRef $ \fr -> do
                idx <- fileReaderParse fr parsePackIndexHeader
                liftM2 (,) (return idx) (fileReaderParse fr (parsePackIndex $ packIndexHeaderGetSize idx))
        where parsePackIndex sz = do
                sha1s     <- V.replicateM (fromIntegral sz) P.referenceBin
                crcs      <- V.replicateM (fromIntegral sz) P.word32
                packoffs  <- V.replicateM (fromIntegral sz) P.word32
                let nbLarge = length $ filter (== True) $ map (\packoff -> packoff `testBit` 31) $ V.toList packoffs
                largeoffs <- replicateM nbLarge (P.takeBytes 4)
                packfileChecksum <- P.referenceBin
                idxfileChecksum  <- P.referenceBin
                -- large packfile offsets
                -- trailer
                return (sha1s, crcs, packoffs, largeoffs, packfileChecksum, idxfileChecksum)
