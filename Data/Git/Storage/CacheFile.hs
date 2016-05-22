{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Git.Storage.CacheFile
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unix
--
module Data.Git.Storage.CacheFile (CacheFile, newCacheVal, getCacheVal) where

import Control.Concurrent.MVar
import qualified Control.Exception as E
import           Data.Git.Imports
import           Data.Git.OS

data CacheFile a = CacheFile
    { cacheFilepath :: LocalPath
    , cacheRefresh  :: IO a
    , cacheIniVal   :: a
    , cacheLock     :: MVar (MTime, a)
    }

newCacheVal :: LocalPath -> IO a -> a -> IO (CacheFile a)
newCacheVal path refresh initialVal =
    CacheFile path refresh initialVal <$> newMVar (MTime timeZero, initialVal)

getCacheVal :: CacheFile a -> IO a
getCacheVal cachefile = modifyMVar (cacheLock cachefile) getOrRefresh
    where getOrRefresh s@(mtime, cachedVal) = do
             cMTime <- tryGetMTime $ cacheFilepath cachefile
             case cMTime of
                  Nothing -> return ((MTime timeZero, cacheIniVal cachefile), cacheIniVal cachefile)
                  Just newMtime | newMtime > mtime -> cacheRefresh cachefile >>= \v -> return ((newMtime, v), v)
                                | otherwise        -> return (s, cachedVal)

tryGetMTime :: LocalPath -> IO (Maybe MTime)
tryGetMTime filepath = (Just <$> getMTime filepath) `E.catch` \(_ :: E.SomeException) -> return Nothing
