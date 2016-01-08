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
    -- * Simple re-export
    , A.anyWord8
    , takeBytes
    , AC.string
    , decimal
    ) where

import           Data.Attoparsec.ByteString (parseWith, Parser, IResult(..))
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Char8 as AC
import qualified Data.Attoparsec.ByteString.Lazy as AL
import           Data.Bits

import qualified Data.ByteString as B

import           Data.Git.Ref
import           Data.Git.Internal

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
