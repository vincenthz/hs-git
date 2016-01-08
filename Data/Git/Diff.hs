-- |
-- Module      : Data.Git.Diff
-- License     : BSD-style
-- Maintainer  : Nicolas DI PRIMA <nicolas@di-prima.fr>
-- Stability   : experimental
-- Portability : unix
--
-- Basic Git diff methods.
--

module Data.Git.Diff
    (
    -- * Basic features
      BlobContent(..)
    , BlobState(..)
    , BlobStateDiff(..)
    , getDiffWith
    -- * Default helpers
    , HitDiff(..)
    , HitFileContent(..)
    , FilteredDiff(..)
    , HitFileRef(..)
    , HitFileMode(..)
    , TextLine(..)
    , defaultDiff
    , getDiff
    ) where

import Data.List (find, filter)
import Data.Char (ord)
import Data.Git
import Data.Git.Repository
import Data.Git.Storage
import Data.Git.Storage.Object
import Data.ByteString.Lazy.Char8 as L

import Data.Algorithm.Patience as AP (Item(..), diff)

-- | represents a blob's content (i.e., the content of a file at a given
-- reference).
data BlobContent = FileContent [L.ByteString] -- ^ Text file's lines
                 | BinaryContent L.ByteString -- ^ Binary content
    deriving (Show)

-- | This is a blob description at a given state (revision)
data BlobState = BlobState
    { bsFilename :: EntPath
    , bsMode     :: ModePerm
    , bsRef      :: Ref
    , bsContent  :: BlobContent
    }
    deriving (Show)

-- | Two 'BlobState' are equal if they have the same filename, i.e.,
--
-- > ((BlobState x _ _ _) == (BlobState y _ _ _)) = (x == y)
instance Eq BlobState where
    (BlobState f1 _ _ _) == (BlobState f2 _ _ _) = (f2 == f1)
    a /= b = not (a == b)

-- | Represents a file state between two revisions
-- A file (a blob) can be present in the first Tree's revision but not in the
-- second one, then it has been deleted. If only in the second Tree's revision,
-- then it has been created. If it is in the both, maybe it has been changed.
data BlobStateDiff = OnlyOld   BlobState
                   | OnlyNew   BlobState
                   | OldAndNew BlobState BlobState

buildListForDiff :: Git -> Ref -> IO [BlobState]
buildListForDiff git ref = do
    commit <- getCommit git ref
    tree   <- resolveTreeish git $ commitTreeish commit
    case tree of
        Just t -> do htree <- buildHTree git t
                     buildTreeList htree []
        _      -> error "cannot build a tree from this reference"
    where
        buildTreeList :: HTree -> EntPath -> IO [BlobState]
        buildTreeList [] _ = return []
        buildTreeList ((d,n,TreeFile r):xs) pathPrefix = do
            content <- catBlobFile r
            let isABinary = isBinaryFile content
            listTail <- buildTreeList xs pathPrefix
            case isABinary of
                False -> return $ (BlobState (entPathAppend pathPrefix n) d r (FileContent $ L.lines content)) : listTail
                True  -> return $ (BlobState (entPathAppend pathPrefix n) d r (BinaryContent content)) : listTail
        buildTreeList ((_,n,TreeDir _ subTree):xs) pathPrefix = do
            l1 <- buildTreeList xs      pathPrefix
            l2 <- buildTreeList subTree (entPathAppend pathPrefix n)
            return $ l1 ++ l2

        catBlobFile :: Ref -> IO L.ByteString
        catBlobFile blobRef = do
            mobj <- getObjectRaw git blobRef True
            case mobj of
                Nothing  -> error "not a valid object"
                Just obj -> return $ oiData obj
        getBinaryStat :: L.ByteString -> Double
        getBinaryStat bs = L.foldl' (\acc w -> acc + if isBin $ ord w then 1 else 0) 0 bs / (fromIntegral $ L.length bs)
            where
                isBin :: Int -> Bool
                isBin i
                    | i >= 0 && i <= 8   = True
                    | i == 12            = True
                    | i >= 14 && i <= 31 = True
                    | otherwise          = False

        isBinaryFile :: L.ByteString -> Bool
        isBinaryFile file = let bs = L.take 512 file
                            in  getBinaryStat bs > 0.0

-- | generate a diff list between two revisions with a given diff helper.
--
-- Useful to extract any kind of information from two different revisions.
-- For example you can get the number of deleted files:
--
-- > getdiffwith f 0 head^ head git
-- >     where f (OnlyOld _) acc = acc+1
-- >           f _           acc = acc
--
-- Or save the list of new files:
--
-- > getdiffwith f [] head^ head git
-- >     where f (OnlyNew bs) acc = (bsFilename bs):acc
-- >           f _            acc = acc
getDiffWith :: (BlobStateDiff -> a -> a) -- ^ diff helper (State -> accumulator -> accumulator)
            -> a                         -- ^ accumulator
            -> Ref                       -- ^ commit reference (the original state)
            -> Ref                       -- ^ commit reference (the new state)
            -> Git                       -- ^ repository
            -> IO a
getDiffWith f acc ref1 ref2 git = do
    commit1 <- buildListForDiff git ref1
    commit2 <- buildListForDiff git ref2
    return $ Prelude.foldr f acc $ doDiffWith commit1 commit2
    where
        doDiffWith :: [BlobState] -> [BlobState] -> [BlobStateDiff]
        doDiffWith []        []        = []
        doDiffWith [bs1]     []        = [OnlyOld bs1]
        doDiffWith []        (bs2:xs2) = (OnlyNew bs2):(doDiffWith [] xs2)
        doDiffWith (bs1:xs1) xs2       =
            let bs2Maybe = Data.List.find (\x -> x == bs1) xs2
            in  case bs2Maybe of
                    Just bs2 -> let subxs2 = Data.List.filter (\x -> x /= bs2) xs2
                                in  (OldAndNew bs1 bs2):(doDiffWith xs1 subxs2)
                    Nothing  -> (OnlyOld bs1):(doDiffWith xs1 xs2)

data TextLine = TextLine
    { lineNumber  :: Integer
    , lineContent :: L.ByteString
    }
instance Eq TextLine where
  a == b = (lineContent a) == (lineContent b)
  a /= b = not (a == b)
instance Ord TextLine where
  compare a b = compare (lineContent a) (lineContent b)
  a <  b     = (lineContent a) < (lineContent b)
  a <= b     = (lineContent a) <= (lineContent b)
  a >  b     = b < a
  a >= b     = b <= a

data FilteredDiff = NormalLine (Item TextLine) | Separator

data HitFileContent = NewBinaryFile
                    | OldBinaryFile
                    | NewTextFile  [TextLine]
                    | OldTextFile  [TextLine]
                    | ModifiedBinaryFile
                    | ModifiedFile [FilteredDiff]
                    | UnModifiedFile

data HitFileMode = NewMode        ModePerm
                 | OldMode        ModePerm
                 | ModifiedMode   ModePerm ModePerm
                 | UnModifiedMode ModePerm

data HitFileRef = NewRef        Ref
                | OldRef        Ref
                | ModifiedRef   Ref Ref
                | UnModifiedRef Ref

-- | This is a proposed diff records for a given file.
-- It contains useful information:
--   * the filename (with its path in the root project)
--   * a file diff (with the Data.Algorithm.Patience method)
--   * the file's mode (i.e. the file priviledge)
--   * the file's ref
data HitDiff = HitDiff
    { hFileName    :: EntPath
    , hFileContent :: HitFileContent
    , hFileMode    :: HitFileMode
    , hFileRef     :: HitFileRef
    }

-- | A default Diff getter which returns all diff information (Mode, Content
-- and Binary) with a context of 5 lines.
--
-- > getDiff = getDiffWith (defaultDiff 5) []
getDiff :: Ref
        -> Ref
        -> Git
        -> IO [HitDiff]
getDiff = getDiffWith (defaultDiff 5) []

-- | A default diff helper. It is an example about how you can write your own
-- diff helper or you can use it if you want to get all of differences.
defaultDiff :: Int           -- ^ Number of line for context
            -> BlobStateDiff
            -> [HitDiff]     -- ^ Accumulator
            -> [HitDiff]     -- ^ Accumulator with a new content
defaultDiff _ (OnlyOld   old    ) acc =
    let oldMode    = OldMode (bsMode old)
        oldRef     = OldRef  (bsRef  old)
        oldContent = case bsContent old of
                         BinaryContent _ -> OldBinaryFile
                         FileContent   l -> OldTextFile (Prelude.zipWith TextLine [1..] l)
    in (HitDiff (bsFilename old) oldContent oldMode oldRef):acc
defaultDiff _ (OnlyNew       new) acc =
    let newMode    = NewMode (bsMode new)
        newRef     = NewRef  (bsRef  new)
        newContent = case bsContent new of
                         BinaryContent _ -> NewBinaryFile
                         FileContent   l -> NewTextFile (Prelude.zipWith TextLine [1..] l)
    in (HitDiff (bsFilename new) newContent newMode newRef):acc
defaultDiff context (OldAndNew old new) acc =
    let mode = if (bsMode old) /= (bsMode new) then ModifiedMode (bsMode old) (bsMode new)
                                               else UnModifiedMode (bsMode new)
        ref = if (bsRef old) == (bsRef new) then UnModifiedRef (bsRef new)
                                            else ModifiedRef (bsRef old) (bsRef new)
    in case (mode, ref) of
           ((UnModifiedMode _), (UnModifiedRef _)) -> acc
           _ -> (HitDiff (bsFilename new) (content ref) mode ref):acc
    where content :: HitFileRef -> HitFileContent
          content (UnModifiedRef _) = UnModifiedFile
          content _                 = createDiff (bsContent old) (bsContent new)

          createDiff :: BlobContent -> BlobContent -> HitFileContent
          createDiff (FileContent a) (FileContent b) =
              let linesA = Prelude.zipWith TextLine [1..] a
                  linesB = Prelude.zipWith TextLine [1..] b
              in ModifiedFile $ diffGetContext context (diff linesA linesB)
          createDiff _ _ = ModifiedBinaryFile

-- Used by diffGetContext
data HitwebAccu = AccuBottom | AccuTop

-- Context filter
diffGetContext :: Int -> [Item TextLine] -> [FilteredDiff]
diffGetContext 0 list = fmap NormalLine list
diffGetContext context list =
    let (_, _, filteredDiff) = Prelude.foldr filterContext (0, AccuBottom, []) list
        theList = removeTrailingBoth filteredDiff
    in case Prelude.head theList of
        (NormalLine (Both l1 _)) -> if (lineNumber l1) > 1 then Separator:theList
                                                           else theList
        _ -> theList
    where -- only keep 'context'. The file is annalyzed from the bottom to the top.
          -- The accumulator here is a tuple3 with (the line counter, the
          -- direction and the list of elements)
          filterContext :: (Item TextLine) -> (Int, HitwebAccu, [FilteredDiff]) -> (Int, HitwebAccu, [FilteredDiff])
          filterContext (Both l1 l2) (c, AccuBottom, acc) =
              if c < context then (c+1, AccuBottom, (NormalLine (Both l1 l2)):acc)
                             else (c  , AccuBottom, (NormalLine (Both l1 l2))
                                                    :((Prelude.take (context-1) acc)
                                                    ++ [Separator]
                                                    ++ (Prelude.drop (context+1) acc)))
          filterContext (Both l1 l2) (c, AccuTop, acc) =
              if c < context then (c+1, AccuTop   , (NormalLine (Both l1 l2)):acc)
                             else (0  , AccuBottom, (NormalLine (Both l1 l2)):acc)
          filterContext element (_, _, acc) =
              (0, AccuTop, (NormalLine element):acc)

          startWithSeparator :: [FilteredDiff] -> Bool
          startWithSeparator [] = False
          startWithSeparator (Separator:_) = True
          startWithSeparator ((NormalLine l):xs) =
              case l of
                  (Both _ _) -> startWithSeparator xs
                  _          -> False

          removeTrailingBoth :: [FilteredDiff] -> [FilteredDiff]
          removeTrailingBoth diffList =
              let test = startWithSeparator diffList
              in  if test then Prelude.tail $ Prelude.dropWhile (\a -> not $ startWithSeparator [a]) diffList
                          else diffList
