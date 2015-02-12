module GitWorkflow
    where

import System.IO
import System.Directory (doesFileExist, createDirectoryIfMissing, removeFile)
import Alfred
import System.Process (readProcess, runInteractiveProcess, waitForProcess)
import System.Exit
import Data.List.Split
import GitDirHistory

currentBranch :: IO (Either String String)
currentBranch = do
  d <- getCurrent
  case d of
    Nothing -> return (Left "please enter working direcotry")
    Just dir -> do
           r <- currentBranchInWorkingDirectory dir
           return r

currentBranchInWorkingDirectory :: String -> IO (Either String String)
currentBranchInWorkingDirectory dir = do
  r <- runGitProcessInWorkingDirectory dir ["status", "-sb"]
  return $fmap formatStatusString r

formatStatusString::String -> String
formatStatusString str = let (l0:_) = lines str
                             (s0:s1:ss) = words l0
                             (p1:_) = splitOn "..." s1 in
                         p1 ++ " " ++ unwords ss
    
runGitProcess::[String] -> IO (Either String String)
runGitProcess args = do
  d <- getCurrent
  case d of
    Nothing -> return (Left "please enter working direcotry")
    Just dir -> runGitProcessInWorkingDirectory dir  args

runGitProcessInWorkingDirectory::String -> [String] -> IO (Either String String)
runGitProcessInWorkingDirectory dir args = do
  (_, stdout, stderr ,h) <- runInteractiveProcess  "git" args  (Just dir) Nothing
  exitCode <- waitForProcess h
  case exitCode of
    ExitSuccess -> do
                  out <- hGetContents stdout
                  return $Right out
    ExitFailure _ -> do
                  err <- hGetContents stderr
                  return $Left err
