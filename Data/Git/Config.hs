-- |
-- Module      : Data.Git.Config
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unix
--
-- config related types and methods.
--
{-# LANGUAGE OverloadedStrings #-}
module Data.Git.Config
    ( Config(..)
    , Section(..)
    -- * reading methods
    , readConfig
    , readGlobalConfig
    -- * methods
    , listSections
    , get
    ) where

import Data.Git.Path
import Data.Git.Imports
import Data.List (find)
import Filesystem.Path.CurrentOS
import Filesystem (getHomeDirectory)
import qualified Data.Set as S

newtype Config = Config [Section]
    deriving (Show,Eq)

data Section = Section
    { sectionName :: String
    , sectionKVs  :: [(String, String)]
    } deriving (Show,Eq)

parseConfig :: String -> Config
parseConfig = Config . reverse . toSections . foldl accSections ([], Nothing) . lines
  where toSections (l,Nothing) = l
        toSections (l,Just s)  = s : l

        -- a new section in the config file
        accSections (sections, mcurrent) ('[':sectNameE)
            | last sectNameE == ']' =
                let sectName = take (length sectNameE - 1) sectNameE
                 in case mcurrent of
                    Nothing      -> (sections, Just $ Section sectName [])
                    Just current -> (sectionFinalize current : sections, Just $ Section sectName [])
            | otherwise             =
                (sections, mcurrent)
        -- a normal line without having any section defined yet
        accSections acc@(_, Nothing) _ = acc
        -- potentially a k-v line in an existing section
        accSections (sections, Just current) kvLine =
            case break (== '=') kvLine of
                (k,'=':v) -> (sections, Just $ sectionAppend current (strip k, strip v))
                (_,_)     -> (sections, Just current) -- not a k = v line
        -- append a key-value
        sectionAppend (Section n l) kv = Section n (kv:l)
        sectionFinalize (Section n l) = Section n $ reverse l

        strip s = dropSpaces $ reverse $ dropSpaces $ reverse s
          where dropSpaces = dropWhile (\c -> c == ' ' || c == '\t')

readConfigPath filepath = parseConfig <$> readFile (encodeString filepath)
readConfig gitRepo = readConfigPath (configPath gitRepo)

readGlobalConfig = getHomeDirectory >>= readConfigPath . (\homeDir -> homeDir </> ".gitconfig")

listSections :: [Config] -> [String]
listSections = S.toList . foldr accSections S.empty
  where accSections (Config sections) set = foldr S.insert set (map sectionName sections)

-- | Get a configuration element in a stack of config file, starting from the top.
get :: [Config] -- ^ stack of config
    -> String   -- ^ section name
    -> String   -- ^ key name
    -> Maybe String
get []            _       _   = Nothing
get (Config c:cs) section key = findOne `mplus` get cs section key
  where findOne = find (\s -> sectionName s == section) c >>= lookup key . sectionKVs
