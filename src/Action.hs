import GitWorkflow
import GitDirHistory
import System.Environment (getArgs)
import Data.List
import System.IO

main = do
  a:_ <- getArgs
  run $words a
      
run::[String] -> IO ()
run [] = putStrLn "no command"
run ("cd":dir:_) = do
  appendDir dir
  return ()
run ("checkout":branch:_) = do
  checkout branch
run ("clearDir":_) = clearHistory

run xs = do
  r <- runGitProcess xs
  case r of
    Right out -> putStr out
    Left err -> putStr err


checkout::String -> IO ()
checkout branch | Just remotePath <- stripPrefix "remotes/" branch = do
  let Just localPath = stripPrefix "origin/" remotePath
  r <- runGitProcess ["checkout", "-b", localPath, "--track", remotePath]
  case r of
    Right out -> putStrLn out
    Left err -> putStrLn err
  
checkout branch | Just _ <- stripPrefix "*" branch = do
  putStrLn "is current branch"

checkout branch = do
  r <- runGitProcess ["checkout", branch]
  case r of
    Right out -> putStrLn out
    Left err -> putStrLn err

