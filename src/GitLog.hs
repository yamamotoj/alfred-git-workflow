module GitLog
    where

import System.IO
import GitWorkflow
import System.Process (readProcess, runInteractiveProcess, waitForProcess)
import System.Exit

readGitProcessInWorkingDirectory::String -> [String] -> IO (Either String String)
readGitProcessInWorkingDirectory dir args = do
  (_, stdout, stderr ,h) <- runInteractiveProcess  "git" args  (Just dir) Nothing
  l <-  hGetLine stdout
  putStrLn l
  l <-  hGetLine stdout
  putStrLn l
  l <-  hGetLine stdout
  putStrLn l
  l <-  hGetLine stdout
  putStrLn l
  l <-  hGetLine stdout
  putStrLn l
  l <-  hGetLine stdout
  putStrLn l
  l <-  hGetLine stdout
  putStrLn l
  l <-  hGetLine stdout
  putStrLn l
  l <-  hGetLine stdout
  putStrLn l
  l <-  hGetLine stdout
  putStrLn l
  l <-  hGetLine stdout
  putStrLn l
  l <-  hGetLine stdout
  putStrLn l
  l <-  hGetLine stdout
  putStrLn l
  l <-  hGetLine stdout
  putStrLn l
  l <-  hGetLine stdout
  putStrLn l
  l <-  hGetLine stdout
  putStrLn l
  l <-  hGetLine stdout
  putStrLn l
  l <-  hGetLine stdout
  putStrLn l
  l <-  hGetLine stdout
  putStrLn l
  l <-  hGetLine stdout
  putStrLn l
  l <-  hGetLine stdout
  putStrLn l
  l <-  hGetLine stdout
  putStrLn l
  l <-  hGetLine stdout
  putStrLn l
  l <-  hGetLine stdout
  putStrLn l
  l <-  hGetLine stdout
  putStrLn l
  l <-  hGetLine stdout
  putStrLn l
  l <-  hGetLine stdout
  putStrLn l
  l <-  hGetLine stdout
  putStrLn l
  l <-  hGetLine stdout
  putStrLn l
  l <-  hGetLine stdout
  putStrLn l
  l <-  hGetLine stdout
  putStrLn l
  l <-  hGetLine stdout
  putStrLn l
  l <-  hGetLine stdout
  putStrLn l
  l <-  hGetLine stdout
  putStrLn l
  l <-  hGetLine stdout
  putStrLn l
  l <-  hGetLine stdout
  putStrLn l
  l <-  hGetLine stdout
  putStrLn l
  l <-  hGetLine stdout
  putStrLn l
  l <-  hGetLine stdout
  putStrLn l
  l <-  hGetLine stdout
  putStrLn l
  l <-  hGetLine stdout
  putStrLn l
  l <-  hGetLine stdout
  putStrLn l
  l <-  hGetLine stdout
  putStrLn l
  l <-  hGetLine stdout
  putStrLn l
  l <-  hGetLine stdout
  putStrLn l
  l <-  hGetLine stdout
  putStrLn l
  l <-  hGetLine stdout
  putStrLn l
  l <-  hGetLine stdout
  putStrLn l
  l <-  hGetLine stdout
  putStrLn l
  l <-  hGetLine stdout
  putStrLn l
  l <-  hGetLine stdout
  putStrLn l
  l <-  hGetLine stdout
  putStrLn l
  l <-  hGetLine stdout
  putStrLn l
  l <-  hGetLine stdout
  putStrLn l
  l <-  hGetLine stdout
  putStrLn l
  l <-  hGetLine stdout
  putStrLn l
  exitCode <- waitForProcess h
  case exitCode of
    ExitSuccess -> do
                  return $Right l
    ExitFailure _ -> do
                  err <- hGetContents stderr
                  return $Left err
