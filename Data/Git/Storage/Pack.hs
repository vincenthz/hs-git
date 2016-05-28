-- |
-- Module      : Data.Git.Storage.Pack
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unix
--
{-# LANGUAGE OverloadedStrings #-}
module Data.Git.Storage.Pack
        ( PackedObjectInfo(..)
        , PackedObjectRaw
        -- * Enumerators of packs
        , packEnumerate
        -- * Helpers to process packs
        , packOpen
        , packClose
        -- * Command for the content of a pack
        , packReadHeader
        , packReadMapAtOffset
        , packReadAtOffset
        , packReadRawAtOffset
        , packEnumerateObjects
        -- * turn a packed object into a
        , packedObjectToObject
        , packObjectFromRaw
        ) where

import Control.Arrow (second)

import Data.Bits
import Data.List
import qualified Data.ByteString.Lazy as L

import qualified Data.Git.Parser as P
import Data.Git.Internal
import Data.Git.Imports
import Data.Git.Path
import Data.Git.Storage.Object
import Data.Git.Delta
import Data.Git.Ref
import Data.Git.Types
import Data.Git.OS
import Data.Git.Storage.FileReader

import Data.Word

type PackedObjectRaw hash = (PackedObjectInfo hash, L.ByteString)

data PackedObjectInfo hash = PackedObjectInfo
        { poiType       :: ObjectType
        , poiOffset     :: Word64
        , poiSize       :: Word64
        , poiActualSize :: Word64
        , poiExtra      :: Maybe (ObjectPtr hash)
        } deriving (Show,Eq)

-- | Enumerate the pack refs available in this repository.
packEnumerate :: HashAlgorithm hash => LocalPath -> IO [Ref hash]
packEnumerate repoPath = map onlyHash . filter isPackFile <$> listDirectoryFilename (repoPath </> "objects" </> "pack")
  where
        isPackFile :: String -> Bool
        isPackFile x = ".pack" `isSuffixOf` x
        onlyHash = fromHexString . takebut 5 . drop 5
        takebut n l = take (length l - n) l

-- | open a pack
packOpen :: LocalPath -> Ref hash -> IO FileReader
packOpen repoPath packRef = openFile (packPath repoPath packRef) ReadMode >>= fileReaderNew False

-- | close a pack
packClose :: FileReader -> IO ()
packClose = fileReaderClose

-- | return the number of entries in this pack
packReadHeader :: LocalPath -> Ref hash -> IO Word32
packReadHeader repoPath packRef =
        withFileReader (packPath repoPath packRef) $ \filereader ->
                fileReaderParse filereader parseHeader
        where parseHeader = do
                packMagic <- P.word32
                when (packMagic /= 0x5041434b) $ error "not a git packfile"
                ver <- P.word32
                when (ver /= 2) $ error ("pack file version not supported: " ++ show ver)
                P.word32

-- | read an object at a specific position using a map function on the objectData
packReadMapAtOffset :: HashAlgorithm hash
                    => FileReader
                    -> Word64
                    -> (L.ByteString -> L.ByteString)
                    -> IO (Maybe (Object hash))
packReadMapAtOffset fr offset mapData = fileReaderSeek fr offset >> getNextObject fr mapData

-- | read an object at a specific position
packReadAtOffset :: HashAlgorithm hash => FileReader -> Word64 -> IO (Maybe (Object hash))
packReadAtOffset fr offset = packReadMapAtOffset fr offset id

-- | read a raw representation at a specific position
packReadRawAtOffset :: HashAlgorithm hash
                    => FileReader
                    -> Word64
                    -> IO (PackedObjectRaw hash)
packReadRawAtOffset fr offset = fileReaderSeek fr offset >> getNextObjectRaw fr

-- | enumerate all objects in this pack and callback to f for reach raw objects
packEnumerateObjects :: HashAlgorithm hash
                     => LocalPath
                     -> Ref hash
                     -> Int
                     -> (PackedObjectRaw hash -> IO a)
                     -> IO ()
packEnumerateObjects repoPath packRef entries f =
        withFileReader (packPath repoPath packRef) $ \filebuffer -> do
                fileReaderSeek filebuffer 12
                parseNext filebuffer entries
        where
                parseNext :: FileReader -> Int -> IO ()
                parseNext _  0    = return ()
                parseNext fr ents = getNextObjectRaw fr >>= f >> parseNext fr (ents-1)

getNextObject :: HashAlgorithm hash
              => FileReader
              -> (L.ByteString -> L.ByteString)
              -> IO (Maybe (Object hash))
getNextObject fr mapData =
        packedObjectToObject . second mapData <$> getNextObjectRaw fr

packedObjectToObject :: HashAlgorithm hash
                     => (PackedObjectInfo hash, L.ByteString)
                     -> Maybe (Object hash)
packedObjectToObject (PackedObjectInfo { poiType = ty, poiExtra = extra }, objData) =
        packObjectFromRaw (ty, extra, objData)

packObjectFromRaw :: HashAlgorithm hash
                  => (ObjectType, Maybe (ObjectPtr hash), L.ByteString)
                  -> Maybe (Object hash)
packObjectFromRaw (TypeCommit, Nothing, objData) = P.maybeParseChunks objectParseCommit (L.toChunks objData)
packObjectFromRaw (TypeTree, Nothing, objData)   = P.maybeParseChunks objectParseTree (L.toChunks objData)
packObjectFromRaw (TypeBlob, Nothing, objData)   = P.maybeParseChunks objectParseBlob (L.toChunks objData)
packObjectFromRaw (TypeTag, Nothing, objData)    = P.maybeParseChunks objectParseTag (L.toChunks objData)
packObjectFromRaw (TypeDeltaOff, Just (PtrOfs o), objData) = toObject . DeltaOfs o <$> deltaRead (L.toChunks objData)
packObjectFromRaw (TypeDeltaRef, Just (PtrRef r), objData) = toObject . DeltaRef r <$> deltaRead (L.toChunks objData)
packObjectFromRaw _                              = error "can't happen unless someone change getNextObjectRaw"

getNextObjectRaw :: HashAlgorithm hash => FileReader -> IO (PackedObjectRaw hash)
getNextObjectRaw fr = do
        sobj       <- fileReaderGetPos fr
        (ty, size) <- fileReaderParse fr parseObjectHeader
        extra      <- case ty of
                TypeDeltaRef -> Just . PtrRef . fromBinary <$> fileReaderGetBS 20 fr
                TypeDeltaOff -> Just . PtrOfs . deltaOffFromList <$> fileReaderGetVLF fr
                _            -> return Nothing
        objData    <- fileReaderInflateToSize fr size
        eobj       <- fileReaderGetPos fr

        return (PackedObjectInfo ty sobj (eobj - sobj) size extra, objData)
        where
                parseObjectHeader = do
                        (m, ty, sz) <- splitFirst <$> P.anyByte
                        size <- if m then (sz +) <$> getNextSize 4 else return sz
                        return (ty, size)
                        where
                                getNextSize n = do
                                        (c, sz) <- splitOther n <$> P.anyByte
                                        if c then (sz +) <$> getNextSize (n+7) else return sz

                                splitFirst :: Word8 -> (Bool, ObjectType, Word64)
                                splitFirst w = (w `testBit` 7, toEnum $ fromIntegral ((w `shiftR` 4) .&. 0x7), fromIntegral (w .&. 0xf))
                                splitOther n w = (w `testBit` 7, fromIntegral (w .&. 0x7f) `shiftL` n)

                deltaOffFromList (x:xs) = foldl' acc (fromIntegral (x `clearBit` 7)) xs
                        where acc a w = ((a+1) `shiftL` 7) + fromIntegral (w `clearBit` 7)
                deltaOffFromList [] = error "cannot happen"
