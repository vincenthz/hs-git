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
                mods <- many (choice [parseParent, parseFirstParent, parseAt])
                return $ Revision p mods
        parseParent = try $ do
                _ <- char '^'
                n <- optionMaybe (many1 digit)
                case n of
                        Nothing -> return $ RevModParent 1
                        Just d  -> return $ RevModParent (read d)
        parseFirstParent = try $
                char '~' >> many1 digit >>= return . RevModParentFirstN . read
        parseAt = try $ do
                _  <- char '@' >> char '{'
                at <- choice [ parseAtType, parseAtDate, parseAtN ]
                _  <- char '}'
                return at
        parseAtType = try $ do
                ty <- choice $ map string ["tree","commit","blob","tag"]
                return $ RevModAtType ty
        parseAtN = try $ do
                many1 digit >>= return . RevModAtN . read
        parseAtDate = try $ do
                many (noneOf "}") >>= return . RevModAtDate
