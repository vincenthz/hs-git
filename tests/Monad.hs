{-# LANGUAGE OverloadedStrings #-}

module Monad
    ( testGitMonadLocal
    ) where

import Control.Applicative
import Control.Exception
import Control.Monad

import Data.Git.Monad
import Data.Git.Types (GitTime(..))
import System.Exit
import qualified System.Hourglass as T

testBranch :: RefName
testBranch = "test/not/push"

catchAll :: IO (Either String a) -> IO ()
catchAll f = do
    r <- catchAll' f
    case r of
        Left  err        -> failWith $ show err
        Right (Left err) -> failWith err
        Right (Right _)  -> putStrLn " test/git/monad [OK]"
  where
    catchAll' :: IO a -> IO (Either SomeException a)
    catchAll' f = try f

    failWith :: String -> IO ()
    failWith msg = do
        putStrLn " test/git/monad [FAILED]"
        putStrLn $ " - " ++ msg
        exitFailure

testGitMonadLocal :: IO ()
testGitMonadLocal = catchAll (withCurrentRepo testGitMonad)

timeCurrentGit :: GitM GitTime
timeCurrentGit = liftGit $ GitTime 
    <$> T.timeCurrent
    <*> T.timezoneCurrent

step :: String -> GitM ()
step = liftGit . putStrLn

testGitMonad :: GitM ()
testGitMonad = do
    t <- timeCurrentGit
    let person = Person
            { personName  = "Hit Test Machinery"
            , personEmail = "hit@snarc.org"
            , personTime  = t
            }
    withBranch person testBranch True (return ()) $ \isFirstCommit -> case isFirstCommit of
        Nothing -> setMessage "Initial commit"
        Just _  -> setMessage "add new commit"
    step " + new branch created"
    withCommit testBranch $ do
        author <- getAuthor
        when (t /= personTime author)
            $ fail "master's commit is not the last commit performed"
    step " + branch has been verified"
