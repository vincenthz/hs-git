{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
-- |
-- Module      : Data.Git.Storage.Object
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unix
--
module Data.Git.Storage.Object
        ( ObjectLocation(..)
        , ObjectType(..)
        , ObjectHeader
        , ObjectData
        , ObjectPtr(..)
        , Object(..)
        , ObjectInfo(..)
        , Objectable(..)
        , objectToType
        , objectTypeMarshall
        , objectTypeUnmarshall
        , objectTypeIsDelta
        , objectIsDelta
        , objectToTree
        , objectToCommit
        , objectToTag
        , objectToBlob
        -- * parsing function
        , treeParse
        , commitParse
        , tagParse
        , blobParse
        , objectParseTree
        , objectParseCommit
        , objectParseTag
        , objectParseBlob
        -- * writing function
        , objectWriteHeader
        , objectWrite
        , objectHash
        ) where

import Data.Git.Ref
import Data.Git.Types
import Data.Git.Imports
import qualified Data.Git.Parser as P

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L

import Data.List (intersperse)
import Data.Word
import Text.Printf

#if MIN_VERSION_bytestring(0,10,0)
import Data.ByteString.Lazy.Builder hiding (word8)
#else
import qualified Data.ByteString.Lazy.Char8 as LC

-- tiny builder interface like for bytestring < 0.10 that
-- use normal lazy bytestring concat.
string7 :: String -> L.ByteString
string7 = LC.pack

byteString :: ByteString -> L.ByteString
byteString = LC.fromChunks . (:[])

toLazyByteString = id
#endif

-- | location of an object in the database
data ObjectLocation hash = NotFound | Loose (Ref hash) | Packed (Ref hash) Word64
        deriving (Show,Eq)

-- | Delta objects points to some others objects in the database
-- either as offset in the pack or as a direct reference.
data ObjectPtr hash = PtrRef (Ref hash) | PtrOfs Word64 deriving (Show,Eq)

type ObjectHeader hash = (ObjectType, Word64, Maybe (ObjectPtr hash))

type ObjectData = L.ByteString

-- | Raw objects infos have an header (type, size, ptr),
-- the data and a pointers chains to parents for resolved objects.
data ObjectInfo hash = ObjectInfo
        { oiHeader :: ObjectHeader hash
        , oiData   :: ObjectData
        , oiChains :: [ObjectPtr hash]
        } deriving (Show,Eq)

-- | describe a git object, that could of 6 differents types:
-- tree, blob, commit, tag and deltas (offset or ref).
-- the deltas one are only available in packs.
data Object hash =
      ObjCommit   (Commit hash)
    | ObjTag      (Tag hash)
    | ObjBlob     (Blob hash)
    | ObjTree     (Tree hash)
    | ObjDeltaOfs (DeltaOfs hash)
    | ObjDeltaRef (DeltaRef hash)
    deriving (Show,Eq)

class Objectable a where
        getType  :: a hash -> ObjectType
        getRaw   :: a hash -> L.ByteString
        isDelta  :: a hash -> Bool
        toObject :: a hash -> Object hash

objectToType :: Object hash -> ObjectType
objectToType (ObjTree _)     = TypeTree
objectToType (ObjBlob _)     = TypeBlob
objectToType (ObjCommit _)   = TypeCommit
objectToType (ObjTag _)      = TypeTag
objectToType (ObjDeltaOfs _) = TypeDeltaOff
objectToType (ObjDeltaRef _) = TypeDeltaRef

objectTypeMarshall :: ObjectType -> String
objectTypeMarshall TypeTree   = "tree"
objectTypeMarshall TypeBlob   = "blob"
objectTypeMarshall TypeCommit = "commit"
objectTypeMarshall TypeTag    = "tag"
objectTypeMarshall _          = error "deltas cannot be marshalled"

objectTypeUnmarshall :: ByteString -> ObjectType
objectTypeUnmarshall "tree"   = TypeTree
objectTypeUnmarshall "blob"   = TypeBlob
objectTypeUnmarshall "commit" = TypeCommit
objectTypeUnmarshall "tag"    = TypeTag
objectTypeUnmarshall _        = error "unknown object type"

objectTypeIsDelta :: ObjectType -> Bool
objectTypeIsDelta TypeDeltaOff = True
objectTypeIsDelta TypeDeltaRef = True
objectTypeIsDelta _            = False

objectIsDelta :: Object hash -> Bool
objectIsDelta (ObjDeltaOfs _) = True
objectIsDelta (ObjDeltaRef _) = True
objectIsDelta _               = False

objectToTree :: Object hash -> Maybe (Tree hash)
objectToTree (ObjTree tree) = Just tree
objectToTree _              = Nothing

objectToCommit :: Object hash -> Maybe (Commit hash)
objectToCommit (ObjCommit commit) = Just commit
objectToCommit _                  = Nothing

objectToTag :: Object hash -> Maybe (Tag hash)
objectToTag (ObjTag tag) = Just tag
objectToTag _            = Nothing

objectToBlob :: Object hash -> Maybe (Blob hash)
objectToBlob (ObjBlob blob) = Just blob
objectToBlob _              = Nothing

octal :: P.Parser Int
octal = B.foldl' step 0 `fmap` P.takeWhile1 isOct
  where isOct w = w >= 0x30 && w <= 0x37
        step a w = a * 8 + fromIntegral (w - 0x30)

modeperm :: P.Parser ModePerm
modeperm = ModePerm . fromIntegral <$> octal

-- | parse a tree content
treeParse :: HashAlgorithm hash => P.Parser (Tree hash)
treeParse = Tree <$> parseEnts
    where parseEnts = P.hasMore >>= \b -> if b then liftM2 (:) parseEnt parseEnts else return []
          parseEnt = liftM3 (,,) modeperm parseEntName (P.byte 0 >> P.referenceBin)
          parseEntName = entName <$> (P.skipASCII ' ' >> P.takeWhile (/= 0))

-- | parse a blob content
blobParse :: P.Parser (Blob hash)
blobParse = (Blob . L.fromChunks . (:[]) <$> P.takeAll)

-- | parse a commit content
commitParse :: HashAlgorithm hash => P.Parser (Commit hash)
commitParse = do
        tree <- P.string "tree " >> P.referenceHex
        P.skipEOL
        parents   <- many parseParentRef
        author    <- P.string "author " >> parsePerson
        committer <- P.string "committer " >> parsePerson
        encoding  <- optional (P.string "encoding " >> P.tillEOL)
        extras    <- many parseExtra
        P.skipEOL
        message <- P.takeAll
        return $ Commit tree parents author committer encoding extras message
        where
                parseParentRef = do
                        tree <- P.string "parent " >> P.referenceHex
                        P.skipEOL
                        return tree
                parseExtra = do
                        b <- P.anyByte
                        if b == 0xa
                            then fail "no extra"
                            else do
                                r <- P.tillEOL
                                P.skipEOL
                                v <- concatLines <$> many (P.string " " *> P.tillEOL <* P.skipEOL)
                                return $ CommitExtra (b `B.cons` r) v
                concatLines = B.concat . intersperse (B.pack [0xa])

-- | parse a tag content
tagParse :: HashAlgorithm hash => P.Parser (Tag hash)
tagParse = do
        object <- P.string "object " >> P.referenceHex
        P.skipEOL
        type_ <- objectTypeUnmarshall <$> (P.string "type " >> P.tillEOL)
        P.skipEOL
        tag   <- P.string "tag " >> P.tillEOL -- PC.takeTill ((==) 0x0a)
        P.skipEOL
        tagger <- P.string "tagger " >> parsePerson
        P.skipEOL
        signature <- P.takeAll
        return $ Tag object type_ tag tagger signature

parsePerson :: P.Parser Person
parsePerson = do
        name <- B.init <$> P.takeUntilASCII '<'
        P.skipASCII '<'
        email <- P.takeUntilASCII '>'
        _ <- P.string "> "
        time <- P.decimal :: P.Parser Integer
        _ <- P.string " "
        timezoneSign <- maybe 1 id <$> optional ((const 1 <$> ascii '+') <|> (const (-1) <$> ascii '-'))
        timezoneFmt  <- P.decimal
        let (h,m)    = timezoneFmt `divMod` 100
            timezone = timezoneSign * (h * 60 + m)
        P.skipEOL
        return $ Person name email (gitTime time timezone)

ascii :: Char -> P.Parser ()
ascii c = P.byte (asciiChar c)

asciiChar :: Char -> Word8
asciiChar c
    | cp < 0x80 = fromIntegral cp
    | otherwise = error ("char " <> show c <> " not valid ASCII")
  where cp = fromEnum c

objectParseTree, objectParseCommit, objectParseTag, objectParseBlob :: HashAlgorithm hash => P.Parser (Object hash)
objectParseTree   = ObjTree <$> treeParse
objectParseCommit = ObjCommit <$> commitParse
objectParseTag    = ObjTag <$> tagParse
objectParseBlob   = ObjBlob <$> blobParse

-- header of loose objects, but also useful for any object to determine object's hash
objectWriteHeader :: ObjectType -> Word64 -> ByteString
objectWriteHeader ty sz = BC.pack (objectTypeMarshall ty ++ " " ++ show sz ++ [ '\0' ])

objectWrite :: Object hash -> L.ByteString
objectWrite (ObjCommit commit) = commitWrite commit
objectWrite (ObjTag tag)       = tagWrite tag
objectWrite (ObjBlob blob)     = blobWrite blob
objectWrite (ObjTree tree)     = treeWrite tree
objectWrite _                  = error "delta cannot be marshalled"

treeWrite :: Tree hash -> L.ByteString
treeWrite (Tree ents) = toLazyByteString $ mconcat $ concatMap writeTreeEnt ents
    where writeTreeEnt (ModePerm perm,name,ref) =
                [ string7 (printf "%o" perm)
                , string7 " "
                , byteString $ getEntNameBytes name
                , string7 "\0"
                , byteString $ toBinary ref
                ]

commitWrite :: Commit hash -> L.ByteString
commitWrite (Commit tree parents author committer encoding extra msg) =
    toLazyByteString $ mconcat els
    where
          toNamedRef s r = mconcat [string7 s, byteString (toHex r),eol]
          toParent       = toNamedRef "parent "
          toCommitExtra (CommitExtra k v) = [byteString k, eol] ++
                                            (concatMap (\l -> [byteString " ", byteString l, eol]) $ linesLast v)

          linesLast b
            | B.length b > 0 && B.last b == 0xa = BC.lines b ++ [ "" ]
            | otherwise                         = BC.lines b
          els = [toNamedRef "tree " tree ]
             ++ map toParent parents
             ++ [byteString $ writeName "author" author, eol
                ,byteString $ writeName "committer" committer, eol
                ,maybe (byteString B.empty) (byteString) encoding -- FIXME need eol
                ]
             ++ concatMap toCommitExtra extra
             ++ [eol
                ,byteString msg
                ]

tagWrite :: Tag hash -> L.ByteString
tagWrite (Tag ref ty tag tagger signature) =
    toLazyByteString $ mconcat els
    where els = [ string7 "object ", byteString (toHex ref), eol
                , string7 "type ", string7 (objectTypeMarshall ty), eol
                , string7 "tag ", byteString tag, eol
                , byteString $ writeName "tagger" tagger, eol
                , eol
                , byteString signature
                ]

eol :: Builder
eol = string7 "\n"

blobWrite :: Blob hash -> L.ByteString
blobWrite (Blob bData) = bData

instance Objectable Blob where
        getType _ = TypeBlob
        getRaw    = blobWrite
        toObject  = ObjBlob
        isDelta   = const False

instance Objectable Commit where
        getType _ = TypeCommit
        getRaw    = commitWrite
        toObject  = ObjCommit
        isDelta   = const False

instance Objectable Tag where
        getType _ = TypeTag
        getRaw    = tagWrite
        toObject  = ObjTag
        isDelta   = const False

instance Objectable Tree where
        getType _ = TypeTree
        getRaw    = treeWrite
        toObject  = ObjTree
        isDelta   = const False

instance Objectable DeltaOfs where
        getType _ = TypeDeltaOff
        getRaw    = error "delta offset cannot be marshalled"
        toObject  = ObjDeltaOfs
        isDelta   = const True

instance Objectable DeltaRef where
        getType _ = TypeDeltaRef
        getRaw    = error "delta ref cannot be marshalled"
        toObject  = ObjDeltaRef
        isDelta   = const True

objectHash :: HashAlgorithm hash => ObjectType -> Word64 -> L.ByteString -> Ref hash
objectHash ty w lbs = hashLBS $ L.fromChunks (objectWriteHeader ty w : L.toChunks lbs)

-- used for objectWrite for commit and tag
writeName :: ByteString -> Person -> ByteString
writeName label (Person name email locTime) =
        B.concat [label, " ", name, " <", email, "> ", BC.pack timeStr]
  where timeStr = show locTime
