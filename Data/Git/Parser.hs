module Data.Git.Parser
    (
    -- * Basic parser functions
      Parser
    , parseWith
    , IResult(..)
    , maybeParseChunks
    , eitherParseChunks
    -- * Specific functions
    , word32
    , ref
    , referenceBin
    , referenceHex
    , vlf
    , tillEOL
    , skipEOL
    , skipASCII
    , takeWhileASCII
    -- * Simple re-export
    , A.anyWord8
    , takeBytes
    , AC.string
    , decimal
    , AL.takeWhile1
    , AL.word8
    , AL.parse
    , AL.maybeResult
    ) where

import           Data.Attoparsec.ByteString (parseWith, Parser, IResult(..))
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Char8 as AC
import qualified Data.Attoparsec.ByteString.Lazy as AL
import           Data.Bits
import           Data.Word (Word8)

import qualified Data.ByteString as B

import           Data.Git.Ref
import           Data.Git.Internal
import           Data.Git.Imports

vlf = do
    bs <- A.takeWhile (\w -> w `testBit` 7)
    l  <- A.anyWord8
    return $ (map (\w -> w `clearBit` 7) $ B.unpack bs) ++ [l]

word32 = be32 <$> A.take 4

ref = referenceBin
referenceBin = fromBinary <$> A.take 20
referenceHex = fromHex <$> A.take 40

decimal :: Parser Int
decimal = AC.decimal

maybeParseChunks f = AL.maybeResult . AL.parse f
eitherParseChunks f = AL.eitherResult . AL.parse f

takeBytes = A.take

takeWhileASCII pred = undefined

tillEOL = A.takeWhile (/= asciiEOL)
skipEOL = A.word8 asciiEOL >> return ()

skipASCII :: Char -> Parser ()
skipASCII c
    | cp < 0x80 = A.word8 (fromIntegral cp) >> return ()
    | otherwise = error ("skipASCII: " ++ show c ++ " not a valid ASCII character")
  where
    cp = fromEnum c
    
asciiEOL :: Word8
asciiEOL = fromIntegral $ fromEnum '\n'
