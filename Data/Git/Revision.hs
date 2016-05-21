-- |
-- Module      : Data.Git.Revision
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unix
--
{-# LANGUAGE DeriveDataTypeable #-}
module Data.Git.Revision
    ( Revision(..)
    , RevModifier(..)
    , RevisionNotFound(..)
    , fromString
    ) where

import Text.Parsec
import Data.String
import Data.Data

-- | A modifier to a revision, which is
-- a function apply of a revision
data RevModifier =
      RevModParent Int       -- ^ parent accessor ^<n> and ^
    | RevModParentFirstN Int -- ^ parent accessor ~<n>
    | RevModAtType String    -- ^ @{type} accessor
    | RevModAtDate String    -- ^ @{date} accessor
    | RevModAtN Int          -- ^ @{n} accessor
    deriving (Eq,Data,Typeable)

instance Show RevModifier where
    show (RevModParent 1)       = "^"
    show (RevModParent n)       = "^" ++ show n
    show (RevModParentFirstN n) = "~" ++ show n
    show (RevModAtType s)       = "@{" ++ s ++ "}"
    show (RevModAtDate s)       = "@{" ++ s ++ "}"
    show (RevModAtN s)          = "@{" ++ show s ++ "}"

-- | A git revision. this can be many things:
--    * a shorten ref
--    * a ref
--    * a named branch or tag
--  followed by optional modifiers 'RevModifier' that can represent:
--    * parenting
--    * type
--    * date
data Revision = Revision String [RevModifier]
    deriving (Eq,Data,Typeable)

-- | Exception when a revision cannot be resolved to a reference
data RevisionNotFound = RevisionNotFound Revision
    deriving (Show,Eq,Data,Typeable)

instance Show Revision where
    show (Revision s ms) = s ++ concatMap show ms

instance IsString Revision where
    fromString = revFromString

revFromString :: String -> Revision
revFromString s = either (error.show) id $ parse parser "" s
  where parser = do
            p    <- many (noneOf "^~@")
            mods <- many (parseParent <|> parseFirstParent <|> parseAt)
            return $ Revision p mods
        parseParent = try $ do
            _ <- char '^'
            n <- optionMaybe (some digit)
            case n of
                Nothing -> return $ RevModParent 1
                Just d  -> return $ RevModParent (read d)
        parseFirstParent = try $
            RevModParentFirstN . read <$> (char '~' *> some digit)
        parseAt = try $ do
            _  <- char '@' >> char '{'
            at <- parseAtType <|> parseAtDate <|> parseAtN
            _  <- char '}'
            return at
        parseAtType = try $ do
            RevModAtType <$> (string "tree" <|> string "commit" <|> string "blob" <|> string "tag")
        parseAtN = try $ do
            RevModAtN . read <$> some digit
        parseAtDate = try $ do
            RevModAtDate <$> many (noneOf "}")

some = many1

{-
eatRet :: Show elem => (elem -> Maybe a) -> Stream elem a
eatRet predicate = Stream $ \el ->
    case el of
        []   -> Left ("empty stream: eating")
        x:xs ->
            case predicate x of
                Just a  -> Right (a, xs)
                Nothing -> Left ("unexpected atom got: " ++ show x)

eat :: Show elem => (elem -> Bool) -> Stream elem ()
eat predicate = Stream $ \el ->
    case el of
        [] -> Left ("empty stream: eating")
        x:xs
            | predicate x -> Right ((), xs)
            | otherwise   -> Left ("unexpected atom got: " ++ show x)

newtype Stream elem a = Stream { runStream :: [elem] -> Either String (a, [elem]) }
instance Functor (Stream elem) where
    fmap f s = Stream $ \e1 -> case runStream s e1 of
        Left err     -> Left err
        Right (a,e2) -> Right (f a, e2)
instance Applicative (Stream elem) where
    pure  = return
    fab <*> fa = Stream $ \e1 -> case runStream fab e1 of
        Left err      -> Left err
        Right (f, e2) -> either Left (Right . first f) $ runStream fa e2
instance Alternative (Stream elem) where
    empty     = Stream $ \_  -> Left "empty"
    f1 <|> f2 = Stream $ \e1 -> either (\_ -> runStream f2 e1) Right $ runStream f1 e1
instance Monad (Stream elem) where
    return a  = Stream $ \e1 -> Right (a, e1)
    ma >>= mb = Stream $ \e1 -> either Left (\(a, e2) -> runStream (mb a) e2) $ runStream ma e1
-} 
