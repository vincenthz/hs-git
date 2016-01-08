-- |
-- Module      : Data.Git.Object
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unix
--
{-# LANGUAGE DeriveDataTypeable #-}
module Data.Git.Types
    (
    -- * Type of types
      ObjectType(..)
    -- * Main git types
    , Tree(..)
    , Commit(..)
    , CommitExtra(..)
    , Blob(..)
    , Tag(..)
    , Person(..)
    , EntName
    , entName
    , EntPath
    , entPathAppend
    -- * modeperm type
    , ModePerm(..)
    , FilePermissions(..)
    , ObjectFileType(..)
    , getPermission
    , getFiletype
    -- * time type
    , GitTime(..)
    , gitTime
    , gitTimeToLocal
    -- * Pack delta types
    , DeltaOfs(..)
    , DeltaRef(..)
    -- * Basic types part of other bigger types
    , TreeEnt
    ) where

import Data.Word
import Data.Bits
import Data.Byteable
import Data.String
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

import Data.Git.Ref
import Data.Git.Delta
import Data.Git.Imports
import Data.Hourglass (Elapsed, TimezoneOffset(..)
                      , timePrint, timeConvert
                      , Time(..), Timeable(..)
                      , LocalTime, localTimeSetTimezone, localTimeFromGlobal)
import Data.Data
import qualified Data.ByteString.UTF8 as UTF8

-- | type of a git object.
data ObjectType =
      TypeTree
    | TypeBlob
    | TypeCommit
    | TypeTag
    | TypeDeltaOff
    | TypeDeltaRef
    deriving (Show,Eq,Data,Typeable)

-- | Git time is number of seconds since unix epoch in the UTC zone with
-- the current timezone associated
data GitTime = GitTime
    { gitTimeUTC      :: Elapsed
    , gitTimeTimezone :: TimezoneOffset
    } deriving (Eq)

instance Timeable GitTime where
    timeGetNanoSeconds _ = 0
    timeGetElapsedP (GitTime t _) = timeConvert t
    timeGetElapsed  (GitTime t _) = t
instance Time GitTime where
    timeFromElapsedP e = GitTime (timeGetElapsed e) (TimezoneOffset 0)
    timeFromElapsed e = GitTime e (TimezoneOffset 0)

instance Show GitTime where
    show (GitTime t tz) =
        timePrint "EPOCH" t ++ " " ++ show tz

gitTime :: Integer -> Int -> GitTime
gitTime seconds tzMins =
    GitTime (fromIntegral seconds) (TimezoneOffset tzMins)

gitTimeToLocal :: GitTime -> LocalTime Elapsed
gitTimeToLocal (GitTime t tz) =
    localTimeSetTimezone tz (localTimeFromGlobal t)

-- | the enum instance is useful when marshalling to pack file.
instance Enum ObjectType where
    fromEnum TypeCommit   = 0x1
    fromEnum TypeTree     = 0x2
    fromEnum TypeBlob     = 0x3
    fromEnum TypeTag      = 0x4
    fromEnum TypeDeltaOff = 0x6
    fromEnum TypeDeltaRef = 0x7

    toEnum 0x1 = TypeCommit
    toEnum 0x2 = TypeTree
    toEnum 0x3 = TypeBlob
    toEnum 0x4 = TypeTag
    toEnum 0x6 = TypeDeltaOff
    toEnum 0x7 = TypeDeltaRef
    toEnum n   = error ("not a valid object: " ++ show n)

newtype ModePerm = ModePerm Word32
    deriving (Show,Eq)

getPermission :: ModePerm -> FilePermissions
getPermission (ModePerm modeperm) =
    let owner = (modeperm .&. 0x700) `shiftR` 6
        group = (modeperm .&. 0x70) `shiftR` 3
        other = modeperm .&. 0x7
     in FilePermissions (fromIntegral owner) (fromIntegral group) (fromIntegral other)

getFiletype :: ModePerm -> ObjectFileType
getFiletype (ModePerm modeperm) =
    case modeperm `shiftR` 12 of
        _ -> error "filetype unknown"

-- | Git object file type
data ObjectFileType =
      FileTypeDirectory
    | FileTypeRegularFile
    | FileTypeSymbolicLink
    | FileTypeGitLink
    deriving (Show,Eq)

-- | traditional unix permission for owner, group and permissions
data FilePermissions = FilePermissions
    { getOwnerPerm :: {-# UNPACK #-} !Perm
    , getGroupPerm :: {-# UNPACK #-} !Perm
    , getOtherPerm :: {-# UNPACK #-} !Perm
    } deriving (Show,Eq)

-- | a bitfield representing a typical unix permission:
-- * bit 0 represents the read permission
-- * bit 1 represents the write permission
-- * bit 2 represents the execute permission
type Perm = Word8

-- | Entity name
newtype EntName = EntName ByteString
    deriving (Eq,Ord)
instance Show EntName where
    show (EntName e) = UTF8.toString e
instance IsString EntName where
    fromString s = entName $ UTF8.fromString s
instance Byteable EntName where
    toBytes (EntName n) = n

entName :: ByteString -> EntName
entName bs
    | B.elem slash bs = error ("entity name " ++ show bs ++ " contains an invalid '/' character")
    | otherwise       = EntName bs
  where slash = 47

entPathAppend :: EntPath -> EntName -> EntPath
entPathAppend l e = l ++ [e]

type EntPath = [EntName]

-- | represent one entry in the tree
-- (permission,file or directory name,blob or tree ref)
-- name should maybe a filepath, but not sure about the encoding.
type TreeEnt = (ModePerm,EntName,Ref)

-- | an author or committer line
-- has the format: name <email> time timezone
-- FIXME: should be a string, but I don't know if the data is stored
-- consistantly in one encoding (UTF8)
data Person = Person
    { personName  :: ByteString
    , personEmail :: ByteString
    , personTime  :: GitTime
    } deriving (Show,Eq)

-- | Represent a root tree with zero to many tree entries.
data Tree = Tree { treeGetEnts :: [TreeEnt] } deriving (Show,Eq)

instance Monoid Tree where
    mempty                      = Tree []
    mappend (Tree e1) (Tree e2) = Tree (e1 ++ e2)
    mconcat trees               = Tree $ concatMap treeGetEnts trees

-- | Represent a binary blob.
data Blob = Blob { blobGetContent :: L.ByteString } deriving (Show,Eq)

-- | Represent a commit object.
data Commit = Commit
    { commitTreeish   :: Ref
    , commitParents   :: [Ref]
    , commitAuthor    :: Person
    , commitCommitter :: Person
    , commitEncoding  :: Maybe ByteString
    , commitExtras    :: [CommitExtra]
    , commitMessage   :: ByteString
    } deriving (Show,Eq)

data CommitExtra = CommitExtra
    { commitExtraKey   :: ByteString
    , commitExtraValue :: ByteString
    } deriving (Show,Eq)

-- | Represent a signed tag.
data Tag = Tag
    { tagRef        :: Ref
    , tagObjectType :: ObjectType
    , tagBlob       :: ByteString
    , tagName       :: Person
    , tagS          :: ByteString
    } deriving (Show,Eq)

-- | Delta pointing to an offset.
data DeltaOfs = DeltaOfs Word64 Delta
    deriving (Show,Eq)

-- | Delta pointing to a ref.
data DeltaRef = DeltaRef Ref Delta
    deriving (Show,Eq)
