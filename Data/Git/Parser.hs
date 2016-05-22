module Data.Git.Parser
    (
      Parser
    , P.Result(..)
    , eitherParse
    , eitherParseChunks
    , maybeParse
    , maybeParseChunks
    -- * Specific functions
    , word32
    , ref
    , referenceBin
    , referenceHex
    , vlf
    , tillEOL
    , skipEOL
    , skipASCII
    , takeUntilASCII
    , decimal
    , takeWhile1
    , string
    -- * Simple re-export
    , P.anyByte
    , P.byte
    , P.bytes
    , P.take
    , P.takeWhile
    , P.parse
    , P.parseFeed
    , P.takeAll
    , P.hasMore
    ) where

import qualified Data.ByteArray.Parse as P
import           Data.ByteArray (ByteArray)

import           Data.Bits
import           Data.Word (Word8, Word32)
import           Data.Char (isDigit)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import           Data.Git.Ref
import           Data.Git.Internal
import           Data.Git.Imports

type Parser = P.Parser B.ByteString

vlf :: Parser [Word8]
vlf = do
    bs <- P.takeWhile (\w -> w `testBit` 7)
    l  <- P.anyByte
    return $ (map (\w -> w `clearBit` 7) $ B.unpack bs) ++ [l]

word32 :: Parser Word32
word32 = be32 <$> P.take 4

ref, referenceBin, referenceHex :: Parser Ref
ref = referenceBin
referenceBin = fromBinary <$> P.take 20
referenceHex = fromHex <$> P.take 40

decimal :: (Read n, Num n) => Parser n
decimal = toNum <$> P.takeWhile (\x -> isDigit $ toEnum (fromIntegral x))
  where toNum = read . BC.unpack

string :: B.ByteString -> Parser ()
string = P.bytes

maybeParse :: Parser a -> B.ByteString -> Maybe a
maybeParse f = toMaybe . P.parse f

maybeParseChunks :: Parser a -> [B.ByteString] -> Maybe a
maybeParseChunks p []     = toMaybe $ P.parse p B.empty
maybeParseChunks p (i:is) = loop (P.parse p i) is
  where
    loop (P.ParseOK _ a) []     = Just a
    loop (P.ParseMore c) []     = toMaybe $ c Nothing
    loop (P.ParseMore c) (x:xs) = loop (c $ Just x) xs
    loop _               _      = Nothing

toMaybe :: P.Result t a -> Maybe a
toMaybe (P.ParseOK _ a) = Just a
toMaybe (P.ParseMore c) = toMaybe (c Nothing)
toMaybe _               = Nothing

eitherParse :: Parser a -> B.ByteString -> Either String a
eitherParse f = toEither . P.parse f

eitherParseChunks :: Show a => Parser a -> [B.ByteString] -> Either String a
eitherParseChunks p []     = toEither $ P.parse p B.empty
eitherParseChunks p (i:is) = loop (P.parse p i) is
  where
    loop (P.ParseOK _ a) []     = Right a
    loop (P.ParseMore c) []     = toEither $ c Nothing
    loop (P.ParseMore c) (x:xs) = loop (c $ Just x) xs
    loop ps              l      = Left ("eitherParseChunk: error: " <> show ps <> " : " <> show l)

toEither :: P.Result t b -> Either String b
toEither (P.ParseOK _ a) = Right a
toEither (P.ParseFail e) = Left e
toEither (P.ParseMore c) = toEither (c Nothing)

takeUntilASCII :: ByteArray byteArray => Char -> P.Parser byteArray byteArray
takeUntilASCII char = P.takeWhile (\c -> if fromEnum c < 0x80 then fromEnum c /= fromEnum char else True)

tillEOL :: Parser B.ByteString
tillEOL = P.takeWhile (/= asciiEOL)

skipEOL :: Parser ()
skipEOL = P.byte asciiEOL >> return ()

skipASCII :: Char -> Parser ()
skipASCII c
    | cp < 0x80 = P.byte (fromIntegral cp) >> return ()
    | otherwise = error ("skipASCII: " ++ show c ++ " not a valid ASCII character")
  where
    cp = fromEnum c

asciiEOL :: Word8
asciiEOL = fromIntegral $ fromEnum '\n'

isByte :: ByteArray byteArray => (Word8 -> Bool) -> P.Parser byteArray Word8
isByte predicate = do
    b <- P.anyByte
    if predicate b then return b else fail "isByte"

takeWhile1 :: (Word8 -> Bool) -> Parser B.ByteString
takeWhile1 predicate =
    B.cons <$> isByte predicate <*> P.takeWhile predicate
