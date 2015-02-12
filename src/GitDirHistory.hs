module GitDirHistory
    where

import System.IO
import System.Directory (doesFileExist, createDirectoryIfMissing, removeFile)
import Alfred
import System.Process (readProcess, runInteractiveProcess, waitForProcess)
import System.Exit
import Control.Applicative
import Control.DeepSeq (deepseq)    

type History = [String]

hPutHistory:: History -> Handle -> IO ()
hPutHistory his h =  do
  let c = unlines his
  hPutStr h c
          
withGitDirFile:: IOMode -> (Handle -> IO r) -> IO r
withGitDirFile mode f = do
  path <- gitDirFilePath
  r <-  withFile path mode f
  return r

gitDirFilePath :: IO String
gitDirFilePath = do
  dir <- getDataPath
  return (dir ++ "/gitdir.txt")
                  
getHistory::IO History
getHistory = do
  path <- gitDirFilePath
  b <- doesFileExist path
  if b then do
         h <- openFile path ReadMode
         c <- hGetContents h
         c `deepseq` hClose h
         let hs = lines c
         return hs
  else return []
  
getCurrent::IO (Maybe String)
getCurrent = do
  hs <- getHistory
  case hs of
    [] -> return Nothing
    top:_ -> return $Just top

appendDir :: String -> IO History
appendDir path = do
  dir <- getDataPath
  createDirectoryIfMissing True dir
  hs <- getHistory
  let newHis =path:(filter (/= path) hs)
  withGitDirFile WriteMode (hPutHistory newHis)
  return newHis

removeDir :: String -> IO History
removeDir path = do
  dir <- getDataPath
  hs <- getHistory
  let newHis =filter (/= path) hs
  withGitDirFile WriteMode (hPutHistory newHis)
  return newHis
                   
clearHistory :: IO ()
clearHistory = do
    dir <- gitDirFilePath
    putStrLn dir
    removeFile dir
      
