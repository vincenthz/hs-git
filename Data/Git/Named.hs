-- |
-- Module      : Data.Git.Named
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unix
--
-- Manipulation of named references
-- * reading packed-refs file
-- * reading single heads/tags/remote file
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Git.Named
    ( RefSpecTy(..)
    , RefContentTy(..)
    , RefName(..)
    , readPackedRefs
    , PackedRefs(..)
    -- * manipulating loosed name references
    , existsRefFile
    , writeRefFile
    , readRefFile
    -- * listings looses name references
    , looseHeadsList
    , looseTagsList
    , looseRemotesList
    ) where

import Data.String
import Data.Git.Path
import Data.Git.Ref
import Data.Git.Imports
import Data.Git.OS
import Data.List (isPrefixOf)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.UTF8 as UTF8

-- | Represent a named specifier.
data RefSpecTy = RefHead
               | RefOrigHead
               | RefFetchHead
               | RefBranch RefName
               | RefTag RefName
               | RefRemote RefName
               | RefPatches String
               | RefStash
               | RefOther String
               deriving (Show,Eq,Ord)

-- | content of a ref file.
data RefContentTy hash =
      RefDirect (Ref hash)
    | RefLink   RefSpecTy
    | RefContentUnknown B.ByteString
    deriving (Show,Eq)

newtype RefName = RefName { refNameRaw :: String }
    deriving (Show,Eq,Ord)

instance IsString RefName where
    fromString s
        | isValidRefName s = RefName s
        | otherwise        = error ("invalid RefName " ++ show s)

isValidRefName :: String -> Bool
isValidRefName s = not (or $ map isBadChar s)
  where isBadChar :: Char -> Bool
        isBadChar c = c <= ' ' || c >= toEnum 0x7f || c `elem` badAscii
        badAscii = [ '~', '^', ':', '\\', '*', '?', '[' ]

toRefTy :: String -> RefSpecTy
toRefTy s
    | "refs/tags/" `isPrefixOf` s    = RefTag $ RefName $ drop 10 s
    | "refs/heads/" `isPrefixOf` s   = RefBranch $ RefName $ drop 11 s
    | "refs/remotes/" `isPrefixOf` s = RefRemote $ RefName $ drop 13 s
    | "refs/patches/" `isPrefixOf` s = RefPatches $ drop 13 s
    | "refs/stash" == s              = RefStash
    | "HEAD" == s                    = RefHead
    | "ORIG_HEAD" == s               = RefOrigHead
    | "FETCH_HEAD" == s              = RefFetchHead
    | otherwise                      = RefOther $ s

fromRefTy :: RefSpecTy -> String
fromRefTy (RefBranch h)  = "refs/heads/" ++ refNameRaw h
fromRefTy (RefTag h)     = "refs/tags/" ++ refNameRaw h
fromRefTy (RefRemote h)  = "refs/remotes/" ++ refNameRaw h
fromRefTy (RefPatches h) = "refs/patches/" ++ h
fromRefTy RefStash       = "refs/stash"
fromRefTy RefHead        = "HEAD"
fromRefTy RefOrigHead    = "ORIG_HEAD"
fromRefTy RefFetchHead   = "FETCH_HEAD"
fromRefTy (RefOther h)   = h

toPath :: LocalPath -> RefSpecTy -> LocalPath
toPath gitRepo (RefBranch h)  = gitRepo </> "refs" </> "heads" </> fromString (refNameRaw h)
toPath gitRepo (RefTag h)     = gitRepo </> "refs" </> "tags" </> fromString (refNameRaw h)
toPath gitRepo (RefRemote h)  = gitRepo </> "refs" </> "remotes" </> fromString (refNameRaw h)
toPath gitRepo (RefPatches h) = gitRepo </> "refs" </> "patches" </> fromString h
toPath gitRepo RefStash       = gitRepo </> "refs" </> "stash"
toPath gitRepo RefHead        = gitRepo </> "HEAD"
toPath gitRepo RefOrigHead    = gitRepo </> "ORIG_HEAD"
toPath gitRepo RefFetchHead   = gitRepo </> "FETCH_HEAD"
toPath gitRepo (RefOther h)   = gitRepo </> fromString h

data PackedRefs a = PackedRefs
    { packedRemotes :: a
    , packedBranchs :: a
    , packedTags    :: a
    }

readPackedRefs :: HashAlgorithm hash
               => LocalPath
               -> ([(RefName, Ref hash)] -> a)
               -> IO (PackedRefs a)
readPackedRefs gitRepo constr = do
    exists <- isFile (packedRefsPath gitRepo)
    if exists then readLines else return $ finalize emptyPackedRefs
  where emptyPackedRefs = PackedRefs [] [] []
        readLines = finalize . foldl accu emptyPackedRefs . BC.lines <$> readBinaryFile (packedRefsPath gitRepo)
        finalize (PackedRefs a b c) = PackedRefs (constr a) (constr b) (constr c)
        accu a l
            | "#" `BC.isPrefixOf` l = a
            | otherwise =
                let (ref, r) = consumeHexRef hashAlg l
                    name     = UTF8.toString $ B.tail r
                 in case toRefTy name of
                        -- accumulate tag, branch and remotes
                        RefTag refname    -> a { packedTags    = (refname, ref) : packedTags a }
                        RefBranch refname -> a { packedBranchs = (refname, ref) : packedBranchs a }
                        RefRemote refname -> a { packedRemotes = (refname, ref) : packedRemotes a }
                        -- anything else that shouldn't be there get dropped on the floor
                        _                 -> a

-- | list all the loose refs available recursively from a directory starting point
listRefs :: LocalPath -> IO [RefName]
listRefs root = listRefsAcc [] root
  where listRefsAcc acc dir = do
            files <- listDirectory dir
            getRefsRecursively dir acc files
        getRefsRecursively _   acc []     = return acc
        getRefsRecursively dir acc (x:xs) = do
            isDir <- isDirectory x
            extra <- if isDir
                        then listRefsAcc [] x
                        else let r = UTF8.toString $ localPathEncode $ stripRoot x
                              in if isValidRefName r
                                    then return [fromString r]
                                    else return []
            getRefsRecursively dir (extra ++ acc) xs
        stripRoot p = maybe (error "stripRoot invalid") id $ stripPrefix root p

looseHeadsList :: LocalPath -> IO [RefName]
looseHeadsList gitRepo = listRefs (headsPath gitRepo)

looseTagsList :: LocalPath -> IO [RefName]
looseTagsList gitRepo = listRefs (tagsPath gitRepo)

looseRemotesList :: LocalPath -> IO [RefName]
looseRemotesList gitRepo = listRefs (remotesPath gitRepo)

existsRefFile :: LocalPath -> RefSpecTy -> IO Bool
existsRefFile gitRepo specty = isFile $ toPath gitRepo specty

writeRefFile :: LocalPath -> RefSpecTy -> RefContentTy hash -> IO ()
writeRefFile gitRepo specty refcont = do
    createParentDirectory filepath
    writeBinaryFile filepath $ fromRefContent refcont
    where filepath = toPath gitRepo specty
          fromRefContent (RefLink link)        = B.concat ["ref: ", UTF8.fromString $ fromRefTy link, B.singleton 0xa]
          fromRefContent (RefDirect ref)       = B.concat [toHex ref, B.singleton 0xa]
          fromRefContent (RefContentUnknown c) = c

readRefFile :: HashAlgorithm hash => LocalPath -> RefSpecTy -> IO (RefContentTy hash)
readRefFile gitRepo specty = toRefContent <$> readBinaryFile filepath
    where filepath = toPath gitRepo specty
          toRefContent content
            | "ref: " `B.isPrefixOf` content = RefLink $ toRefTy $ UTF8.toString $ head $ BC.lines $ B.drop 5 content
            | B.length content < 42          = RefDirect $ fst $ consumeHexRef hashAlg content
            | otherwise                      = RefContentUnknown content

consumeHexRef :: HashAlgorithm hash => hash -> B.ByteString -> (Ref hash, B.ByteString)
consumeHexRef alg b = let (b1,b2) = B.splitAt (hashDigestSize alg * 2) b in (fromHex b1, b2)
