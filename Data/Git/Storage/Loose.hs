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
import Data.Git.Imports
import Data.Git.Storage.FileWriter
import Data.Git.Storage.Object
import qualified Data.Git.Parser as P

import Filesystem
import Filesystem.Path
import qualified Filesystem.Path.Rules as Rules

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B

import Data.Attoparsec.Lazy
import Control.Exception (onException, SomeException)
import qualified Control.Exception as E

import Data.String
import Data.Char (isHexDigit)

import Prelude hiding (FilePath)

isObjectPrefix [a,b] = isHexDigit a && isHexDigit b
isObjectPrefix _     = False

-- loose object parsing
parseHeader = do
        h <- takeWhile1 ((/=) 0x20)
        _ <- word8 0x20
        sz <- P.decimal
        return (objectTypeUnmarshall h, fromIntegral sz, Nothing)

parseTreeHeader   = P.string "tree " >> P.decimal >> word8 0
parseTagHeader    = P.string "tag " >> P.decimal >> word8 0
parseCommitHeader = P.string "commit " >> P.decimal >> word8 0
parseBlobHeader   = P.string "blob " >> P.decimal >> word8 0

parseTree   = parseTreeHeader >> objectParseTree
parseTag    = parseTagHeader >> objectParseTag
parseCommit = parseCommitHeader >> objectParseCommit
parseBlob   = parseBlobHeader >> objectParseBlob

parseObject :: L.ByteString -> Object
parseObject = parseSuccess (parseTree <|> parseBlob <|> parseCommit <|> parseTag)
        where parseSuccess p = either error id . P.eitherParseChunks  p

-- | unmarshall an object (with header) from a bytestring.
looseUnmarshall :: L.ByteString -> Object
looseUnmarshall = parseObject

-- | unmarshall an object (with header) from a zipped stream.
looseUnmarshallZipped :: Zipped -> Object
looseUnmarshallZipped = parseObject . dezip

-- | unmarshall an object as (header, data) tuple from a bytestring
looseUnmarshallRaw :: L.ByteString -> (ObjectHeader, ObjectData)
looseUnmarshallRaw stream =
        case L.findIndex ((==) 0) stream of
                Nothing  -> error "object not right format. missing 0"
                Just idx ->
                        let (h, r) = L.splitAt (idx+1) stream in
                        case maybeResult $ parse parseHeader h of
                                Nothing  -> error "cannot open object"
                                Just hdr -> (hdr, r)

-- | unmarshall an object as (header, data) tuple from a zipped stream
looseUnmarshallZippedRaw :: Zipped -> (ObjectHeader, ObjectData)
looseUnmarshallZippedRaw = looseUnmarshallRaw . dezip

-- | read a specific ref from a loose object and returns an header and data.
looseReadRaw repoPath ref = looseUnmarshallZippedRaw <$> readZippedFile (objectPathOfRef repoPath ref)

-- | read only the header of a loose object.
looseReadHeader repoPath ref = toHeader <$> readZippedFile (objectPathOfRef repoPath ref)
        where toHeader = either error id . P.eitherParseChunks parseHeader . dezip

-- | read a specific ref from a loose object and returns an object
looseRead repoPath ref = looseUnmarshallZipped <$> readZippedFile (objectPathOfRef repoPath ref)

-- | check if a specific ref exists as loose object
looseExists repoPath ref = isFile (objectPathOfRef repoPath ref)

-- | enumarate all prefixes available in the object store.
looseEnumeratePrefixes repoPath = filter isObjectPrefix <$> getDirectoryContents (repoPath </> fromString "objects")

-- | enumerate all references available with a specific prefix.
looseEnumerateWithPrefixFilter :: FilePath -> String -> (Ref -> Bool) -> IO [Ref]
looseEnumerateWithPrefixFilter repoPath prefix filterF =
        filter filterF . map (fromHexString . (prefix ++)) . filter isRef <$> getDir (repoPath </> fromString "objects" </> fromString prefix)
        where
                getDir p = E.catch (getDirectoryContents p) (\(_::SomeException) -> return [])
                isRef l = length l == 38

looseEnumerateWithPrefix :: FilePath -> String -> IO [Ref]
looseEnumerateWithPrefix repoPath prefix =
        looseEnumerateWithPrefixFilter repoPath prefix (const True)

-- | marshall as lazy bytestring an object except deltas.
looseMarshall obj
        | objectIsDelta obj = error "cannot write delta object loose"
        | otherwise         = L.concat [ L.fromChunks [hdrB], objData ]
        where
                objData = objectWrite obj
                hdrB    = objectWriteHeader (objectToType obj) (fromIntegral $ L.length objData)

-- | create a new blob on a temporary location and on success move it to
-- the object store with its digest name.
looseWriteBlobFromFile repoPath file = do
        fsz <- getSize file
        let hdr = objectWriteHeader TypeBlob (fromIntegral fsz)
        tmpPath <- objectTemporaryPath repoPath
        flip onException (removeFile tmpPath) $ do
                (ref, npath) <- withFileWriter tmpPath $ \fw -> do
                        fileWriterOutput fw hdr
                        withFile file ReadMode $ \h -> loop h fw
                        digest <- fileWriterGetDigest fw
                        return (digest, objectPathOfRef repoPath digest)
                exists <- isFile npath
                when exists $ error "destination already exists"
                rename tmpPath npath
                return ref
    where loop h fw = do
                r <- B.hGet h (32*1024)
                if B.null r
                    then return ()
                    else fileWriterOutput fw r >> loop h fw

-- | write an object to disk as a loose reference.
-- use looseWriteBlobFromFile for efficiently writing blobs when being commited from a file.
looseWrite repoPath obj = createDirectory True (directory path)
                       >> isFile path
                       >>= \exists -> unless exists (writeFileLazy path $ compress content)
                       >> return ref
        where
                path    = objectPathOfRef repoPath ref
                content = looseMarshall obj
                ref     = hashLBS content
                writeFileLazy p bs = withFile p WriteMode (\h -> L.hPut h bs)

getDirectoryContents p = map (Rules.encodeString Rules.posix . filename) <$> listDirectory p
