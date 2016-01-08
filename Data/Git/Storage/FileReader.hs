-- |
-- Module      : Data.Git.Storage.FileReader
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unix
--
{-# LANGUAGE DeriveDataTypeable #-}
module Data.Git.Storage.FileReader
        ( FileReader
        , fileReaderNew
        , fileReaderClose
        , withFileReader
        , withFileReaderDecompress
        , fileReaderGetPos
        , fileReaderGet
        , fileReaderGetLBS
        , fileReaderGetBS
        , fileReaderGetVLF
        , fileReaderSeek
        , fileReaderParse

        , fileReaderInflateToSize
        ) where


import Control.Exception (bracket, throwIO)

import Data.ByteString (ByteString)
import Data.ByteString.Unsafe
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import Data.IORef

import Data.Git.Imports
import qualified Data.Git.Parser as P

import Data.Data
import Data.Word

import Codec.Zlib
import Codec.Zlib.Lowlevel
import Foreign.ForeignPtr
import qualified Control.Exception as E

import System.IO (hClose, hSeek, SeekMode(..))
import Filesystem
import Prelude hiding (FilePath)

data FileReader = FileReader
        { fbHandle     :: Handle
        , fbUseInflate :: Bool
        , fbInflate    :: Inflate
        , fbRemaining  :: IORef ByteString
        , fbPos        :: IORef Word64
        }

data InflateException = InflateException Word64 Word64 String
    deriving (Show,Eq,Typeable)

instance E.Exception InflateException

fileReaderNew :: Bool -> Handle -> IO FileReader
fileReaderNew decompress handle = do
        ref <- newIORef B.empty
        pos <- newIORef 0
        inflate <- initInflate defaultWindowBits
        return $ FileReader handle decompress inflate ref pos

fileReaderClose :: FileReader -> IO ()
fileReaderClose = hClose . fbHandle

withFileReader :: FilePath -> (FileReader -> IO a) -> IO a
withFileReader path f =
        bracket (openFile path ReadMode) (hClose) $ \handle ->
                bracket (fileReaderNew False handle) (\_ -> return ()) f

withFileReaderDecompress :: FilePath -> (FileReader -> IO a) -> IO a
withFileReaderDecompress path f =
        bracket (openFile path ReadMode) (hClose) $ \handle ->
                bracket (fileReaderNew True handle) (\_ -> return ()) f

fileReaderGetNext :: FileReader -> IO ByteString
fileReaderGetNext fb = do
        bs <- if fbUseInflate fb then inflateTillPop else B.hGet (fbHandle fb) 8192
        modifyIORef (fbPos fb) (\pos -> pos + (fromIntegral $ B.length bs))
        return bs
        where inflateTillPop = do
                b <- B.hGet (fbHandle fb) 4096
                if B.null b
                        then finishInflate (fbInflate fb)
                        else (>>= maybe inflateTillPop return) =<< feedInflate (fbInflate fb) b

fileReaderGetPos :: FileReader -> IO Word64
fileReaderGetPos fr = do
        storeLeft <- B.length <$> readIORef (fbRemaining fr)
        pos       <- readIORef (fbPos fr)
        return (pos - fromIntegral storeLeft)

fileReaderFill :: FileReader -> IO ()
fileReaderFill fb = fileReaderGetNext fb >>= writeIORef (fbRemaining fb)

fileReaderGet :: Int -> FileReader -> IO [ByteString]
fileReaderGet size fb@(FileReader { fbRemaining = ref }) = loop size
        where loop left = do
                b <- readIORef ref
                if B.length b >= left
                        then do
                                let (b1, b2) = B.splitAt left b
                                writeIORef ref b2
                                return [b1]
                        else do
                                let nleft = left - B.length b
                                fileReaderFill fb
                                liftM (b :) (loop nleft)

fileReaderGetLBS :: Int -> FileReader -> IO L.ByteString
fileReaderGetLBS size fb = L.fromChunks <$> fileReaderGet size fb

fileReaderGetBS :: Int -> FileReader -> IO ByteString
fileReaderGetBS size fb = B.concat <$> fileReaderGet size fb

-- | seek in a handle, and reset the remaining buffer to empty.
fileReaderSeek :: FileReader -> Word64 -> IO ()
fileReaderSeek (FileReader { fbHandle = handle, fbRemaining = ref, fbPos = pos }) absPos = do
        writeIORef ref B.empty >> writeIORef pos absPos >> hSeek handle AbsoluteSeek (fromIntegral absPos)

-- | parse from a filebuffer
fileReaderParse :: FileReader -> P.Parser a -> IO a
fileReaderParse fr@(FileReader { fbRemaining = ref }) parseF = do
        initBS <- readIORef ref
        result <- P.parseWith (fileReaderGetNext fr) parseF initBS
        case result of
                P.Done remaining a -> writeIORef ref remaining >> return a
                P.Partial _        -> error "parsing failed: partial with a handle, reached EOF ?"
                P.Fail _ ctxs err  -> error ("parsing failed: " ++ show ctxs ++ " : " ++ show err)

-- | get a Variable Length Field. get byte as long as MSB is set, and one byte after
fileReaderGetVLF :: FileReader -> IO [Word8]
fileReaderGetVLF fr = fileReaderParse fr P.vlf

fileReaderInflateToSize :: FileReader -> Word64 -> IO L.ByteString
fileReaderInflateToSize fb@(FileReader { fbRemaining = ref }) outputSize = do
        --pos <- fileReaderGetPos fb
        --putStrLn ("inflate to size " ++ show outputSize ++ " " ++ show pos)
        inflate <- inflateNew
        l       <- loop inflate outputSize
        --posend <- fileReaderGetPos fb
        --putStrLn ("inflated input " ++ show posend)
        return $ L.fromChunks l
        where loop inflate left = do
                rbs <- readIORef ref
                let maxToInflate = min left (16 * 1024)
                let lastBlock = if left == maxToInflate then True else False
                (dbs,remaining) <- inflateToSize inflate (fromIntegral maxToInflate) lastBlock rbs (fileReaderGetNext fb)
                                   `E.catch` augmentAndRaise left
                writeIORef ref remaining
                let nleft = left - fromIntegral (B.length dbs)
                if nleft > 0
                        then liftM (dbs:) (loop inflate nleft)
                        else return [dbs]
              augmentAndRaise :: Word64 -> E.SomeException -> IO a
              augmentAndRaise left exn = throwIO $ InflateException outputSize left (show exn)

-- lowlevel helpers to inflate only to a specific size.

inflateNew = do
        zstr <- zstreamNew
        inflateInit2 zstr defaultWindowBits
        newForeignPtr c_free_z_stream_inflate zstr

inflateToSize inflate sz isLastBlock ibs nextBs = withForeignPtr inflate $ \zstr -> do
        let boundSz = min defaultChunkSize sz
        -- create an output buffer
        fbuff <- mallocForeignPtrBytes boundSz
        withForeignPtr fbuff $ \buff -> do
                c_set_avail_out zstr buff (fromIntegral boundSz)
                rbs <- loop zstr ibs
                bs  <- B.packCStringLen (buff, boundSz)
                return (bs, rbs)
        where
                loop zstr nbs = do
                        (ai, streamEnd) <- inflateOneInput zstr nbs
                        ao <- c_get_avail_out zstr
                        if (isLastBlock && streamEnd) || (not isLastBlock && ao == 0)
                                then return $ bsTakeLast ai nbs
                                else do
                                        --when (ai /= 0) $ error ("input not consumed completly: ai" ++ show ai)
                                        (if ai == 0
                                                then nextBs
                                                else return (bsTakeLast ai nbs)) >>= loop zstr

                inflateOneInput zstr bs = unsafeUseAsCStringLen bs $ \(istr, ilen) -> do
                        c_set_avail_in zstr istr $ fromIntegral ilen
                        r <- c_call_inflate_noflush zstr
                        when (r < 0 && r /= (-5)) $ do
                                throwIO $ ZlibException $ fromIntegral r
                        ai <- c_get_avail_in zstr
                        return (ai, r == 1)
                bsTakeLast len bs = B.drop (B.length bs - fromIntegral len) bs
