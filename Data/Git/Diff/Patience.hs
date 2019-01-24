-- loosely based on the patience-0.1.1 package which is:
--
--     Copyright (c) Keegan McAllister 2011
--
module Data.Git.Diff.Patience
    ( Item(..)
    , diff
    ) where

import           Data.List
import           Data.Function (on)
import qualified Data.Map      as M
import qualified Data.IntMap   as IM

data Card a = Card !Int a !(Maybe (Card a))

-- sort using patience making stack of card with the list of elements,
-- then take the highest stack (maxView) and flatten the path back into a list
-- to get the longest increasing path
longestIncreasing :: [(Int,a)] -> [(Int,a)]
longestIncreasing =
      maybe [] (flatten . head . fst)
    . IM.maxView
    . foldl' ins IM.empty
  where
    ins :: IM.IntMap [Card a] -> (Int, a) -> IM.IntMap [Card a]
    ins m (x,a) =
        case IM.minViewWithKey gt of
            Nothing        -> IM.insert x [new] m
            Just ((k,_),_) ->
                case IM.updateLookupWithKey (\_ _ -> Nothing) k m of
                    (Just v, mm) -> IM.insert x (new : v) mm
                    (Nothing, _) -> m
      where
            (lt, gt) = IM.split x m
            prev = (head . fst) `fmap` IM.maxView lt
            new  = Card x a prev

    flatten :: Card a -> [(Int, a)]
    flatten (Card x a c) = (x,a) : maybe [] flatten c

-- Type for decomposing a diff problem.  We either have two
-- lines that match, or a recursive subproblem.
data Piece a =
      Match !a !a
    | Diff [a] [a]
    deriving (Show)

-- Get the longest common subsequence
lcs :: Ord t => [t] -> [t] -> [Piece t]
lcs ma mb =
      chop ma mb
    $ longestIncreasing
    $ sortBy (compare `on` snd)
    $ M.elems
    $ M.intersectionWith (,) (unique ma) (unique mb)
  where
    unique = M.mapMaybe id . foldr ins M.empty . zip [0..]
      where
        ins (a,x) = M.insertWith (\_ _ -> Nothing) x (Just a)

    -- Subdivides a diff problem according to the indices of matching lines.
    chop :: [t] -> [t] -> [(Int,Int)] -> [Piece t]
    chop xs ys []
        | null xs && null ys = []
        | otherwise = [Diff xs ys]
    chop xs ys ((nx,ny):ns) =
        let (xsr, (x : xse)) = splitAt nx xs
            (ysr, (y : yse)) = splitAt ny ys
         in  Diff xse yse : Match x y : chop xsr ysr ns

-- | An element of a computed difference.
data Item t =
      Old  !t
    | New  !t
    | Both !t !t
    deriving (Show,Eq)

instance Functor Item where
    fmap f (Old  x)   = Old  (f x)
    fmap f (New  x)   = New  (f x)
    fmap f (Both x y) = Both (f x) (f y)

-- | The difference between two lists using the patience algorithm
diff :: Ord t => [t] -> [t] -> [Item t]
diff = matchPrefix []
  where
    -- match the prefix between old and new document
    matchPrefix acc (x:xs) (y:ys)
        | x == y    = Both x y : matchPrefix acc xs ys
    matchPrefix acc l r = matchSuffix acc (reverse l) (reverse r)

    -- match the suffix between old and new document, accumulating the
    -- matched item in a reverse accumulator to keep TCO
    matchSuffix acc (x:xs) (y:ys)
        | x == y = matchSuffix (Both x y : acc) xs ys
    matchSuffix acc l r = diffInner (reverse acc) (reverse l) (reverse r)

    -- prefix and suffix are striped, and now do the LCS
    diffInner acc l r =
        case lcs l r of
            -- If we fail to subdivide, just record the chunk as is.
            [Diff _ _] -> fmap Old l ++ fmap New r ++ acc
            ps -> recur acc ps

    recur acc [] = acc
    recur acc (Match x y  : ps) = recur (Both x y : acc) ps
    recur acc (Diff xs ys : ps) = recur [] ps ++ matchPrefix acc xs ys
