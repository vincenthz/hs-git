-- |
-- Module      : Data.Git.Storage.Loose
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unix
--
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, ViewPatterns #-}
module Data.Git.Storage.Loose
        (
          Zipped(..)
        -- * marshall from and to lazy bytestring
        , looseUnmarshall
        , looseUnmarshallRaw
        , looseUnmarshallZipped
        , looseUnmarshallZippedRaw
        , looseMarshall
        -- * read and check object existence
        , looseRead
        , looseReadHeader
        , looseReadRaw
        , looseExists
        -- * write objects
        , looseWriteBlobFromFile
        , looseWrite
        -- * enumeration of loose objects
        , looseEnumeratePrefixes
        , looseEnumerateWithPrefixFilter
        , looseEnumerateWithPrefix
        ) where

import Codec.Compression.Zlib
import Data.Git.Ref
import Data.Git.Path
import Data.Git.Internal
import Data.Git.OS
import Data.Git.Imports
import Data.Git.Storage.FileWriter
import Data.Git.Storage.Object
import qualified Data.Git.Parser as P

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B

import Control.Exception (onException, SomeException)
import qualified Control.Exception as E

import Data.String
import Data.Char (isHexDigit)

newtype Zipped = Zipped { getZippedData :: L.ByteString }
    deriving (Show,Eq)

readZippedFile :: LocalPath -> IO Zipped
readZippedFile fp = Zipped <$> readBinaryFileLazy fp

dezip :: Zipped -> L.ByteString
dezip = decompress . getZippedData

isObjectPrefix :: [Char] -> Bool
isObjectPrefix [a,b] = isHexDigit a && isHexDigit b
isObjectPrefix _     = False

-- loose object parsing
parseHeader :: P.Parser (ObjectHeader hash)
parseHeader = do
        h <- P.takeWhile1 ((/=) 0x20)
        _ <- P.byte 0x20
        sz <- P.decimal :: P.Parser Int
        return (objectTypeUnmarshall h, fromIntegral sz, Nothing)

data HeaderType = HeaderTree | HeaderTag | HeaderCommit | HeaderBlob

parseTreeHeader, parseTagHeader, parseCommitHeader, parseBlobHeader :: P.Parser HeaderType
parseTreeHeader   = P.string "tree " >> parseLength >> P.byte 0 >> return HeaderTree
parseTagHeader    = P.string "tag " >> parseLength >> P.byte 0 >> return HeaderTag
parseCommitHeader = P.string "commit " >> parseLength >> P.byte 0 >> return HeaderCommit
parseBlobHeader   = P.string "blob " >> parseLength >> P.byte 0 >> return HeaderBlob

parseLength :: P.Parser Int
parseLength = P.decimal

parseObject :: HashAlgorithm hash => L.ByteString -> Object hash
parseObject = parseSuccess getOne
  where
    parseSuccess p = either (error . ("parseObject: " ++)) id . P.eitherParseChunks p . L.toChunks
    getOne = do
        hdrType <- parseTreeHeader <|> parseBlobHeader <|> parseCommitHeader <|> parseTagHeader
        case hdrType of
            HeaderTree   -> objectParseTree
            HeaderTag    -> objectParseTag
            HeaderCommit -> objectParseCommit
            HeaderBlob   -> objectParseBlob


-- | unmarshall an object (with header) from a bytestring.
looseUnmarshall :: HashAlgorithm hash => L.ByteString -> Object hash
looseUnmarshall = parseObject

-- | unmarshall an object (with header) from a zipped stream.
looseUnmarshallZipped :: HashAlgorithm hash => Zipped -> Object hash
looseUnmarshallZipped = parseObject . dezip

-- | unmarshall an object as (header, data) tuple from a bytestring
looseUnmarshallRaw :: L.ByteString -> (ObjectHeader hash, ObjectData)
looseUnmarshallRaw stream =
        case L.findIndex ((==) 0) stream of
                Nothing  -> error "object not right format. missing 0"
                Just idx ->
                        let (h, r) = L.splitAt (idx+1) stream in
                        case P.maybeParseChunks parseHeader (L.toChunks h) of
                                Nothing  -> error "cannot open object"
                                Just hdr -> (hdr, r)

-- | unmarshall an object as (header, data) tuple from a zipped stream
looseUnmarshallZippedRaw :: Zipped -> (ObjectHeader hash, ObjectData)
looseUnmarshallZippedRaw = looseUnmarshallRaw . dezip

-- | read a specific ref from a loose object and returns an header and data.
looseReadRaw :: HashAlgorithm hash => LocalPath -> Ref hash -> IO (ObjectHeader hash, ObjectData)
looseReadRaw repoPath ref = looseUnmarshallZippedRaw <$> readZippedFile (objectPathOfRef repoPath ref)

-- | read only the header of a loose object.
looseReadHeader :: HashAlgorithm hash => LocalPath -> Ref hash -> IO (ObjectHeader hash)
looseReadHeader repoPath ref = toHeader <$> readZippedFile (objectPathOfRef repoPath ref)
  where
    toHeader = either (error . ("parseHeader: " ++)) id . P.eitherParseChunks parseHeader . L.toChunks . dezip

-- | read a specific ref from a loose object and returns an object
looseRead :: HashAlgorithm hash => LocalPath -> Ref hash -> IO (Object hash)
looseRead repoPath ref = looseUnmarshallZipped <$> readZippedFile (objectPathOfRef repoPath ref)

-- | check if a specific ref exists as loose object
looseExists :: HashAlgorithm hash => LocalPath -> Ref hash -> IO Bool
looseExists repoPath ref = doesFileExist (objectPathOfRef repoPath ref)

-- | enumarate all prefixes available in the object store.
looseEnumeratePrefixes :: LocalPath -> IO [[Char]]
looseEnumeratePrefixes repoPath = filter isObjectPrefix <$> getDirectoryContents (repoPath </> fromString "objects")

-- | enumerate all references available with a specific prefix.
looseEnumerateWithPrefixFilter :: HashAlgorithm hash => LocalPath -> String -> (Ref hash -> Bool) -> IO [Ref hash]
looseEnumerateWithPrefixFilter repoPath prefix filterF =
        filter filterF . map (fromHexString . (prefix ++)) . filter isRef <$> getDir (repoPath </> fromString "objects" </> fromString prefix)
        where
                getDir p = E.catch (getDirectoryContents p) (\(_::SomeException) -> return [])
                isRef l = length l == 38

looseEnumerateWithPrefix :: HashAlgorithm hash => LocalPath -> String -> IO [Ref hash]
looseEnumerateWithPrefix repoPath prefix =
        looseEnumerateWithPrefixFilter repoPath prefix (const True)

-- | marshall as lazy bytestring an object except deltas.
looseMarshall :: Object hash -> L.ByteString
looseMarshall obj
        | objectIsDelta obj = error "cannot write delta object loose"
        | otherwise         = L.concat [ L.fromChunks [hdrB], objData ]
        where
                objData = objectWrite obj
                hdrB    = objectWriteHeader (objectToType obj) (fromIntegral $ L.length objData)

-- | create a new blob on a temporary location and on success move it to
-- the object store with its digest name.
looseWriteBlobFromFile :: HashAlgorithm hash => LocalPath -> LocalPath -> IO (Ref hash)
looseWriteBlobFromFile repoPath file = do
        fsz <- getFileSize file
        let hdr = objectWriteHeader TypeBlob (fromIntegral fsz)
        tmpPath <- objectTemporaryPath repoPath
        flip onException (removeFile tmpPath) $ do
                (ref, npath) <- withFileWriter tmpPath $ \fw -> do
                        fileWriterOutput fw hdr
                        withFile file ReadMode $ \h -> loop h fw
                        digest <- fileWriterGetDigest fw
                        return (digest, objectPathOfRef repoPath digest)
                exists <- doesFileExist npath
                when exists $ error "destination already exists"
                renameFile tmpPath npath
                return ref
    where loop h fw = do
                r <- B.hGet h (32*1024)
                if B.null r
                    then return ()
                    else fileWriterOutput fw r >> loop h fw

-- | write an object to disk as a loose reference.
-- use looseWriteBlobFromFile for efficiently writing blobs when being commited from a file.
looseWrite :: HashAlgorithm hash => LocalPath -> Object hash -> IO (Ref hash)
looseWrite repoPath obj = createParentDirectory path
                       >> doesFileExist path
                       >>= \exists -> unless exists (writeFileLazy path $ compress content)
                       >> return ref
        where
                path    = objectPathOfRef repoPath ref
                content = looseMarshall obj
                ref     = hashLBS content
                writeFileLazy p bs = withFile p WriteMode (\h -> L.hPut h bs)

getDirectoryContents :: LocalPath -> IO [String]
getDirectoryContents p = listDirectoryFilename p
