module GitLog
    where

import System.IO
import GitWorkflow
import System.Process (readProcess, runInteractiveProcess, waitForProcess)
import System.Exit
type CommitHash = String
data GitLog = GitLog { commit :: CommitHash
                     , author :: String
                     , date :: String
                     , comment:: String
                     }
    

readLog:: IO[String]
readLog = do
  r <- runGitProcess ["log"]
  case r of
    Right logs -> return [logs]
    Left _ -> return []


              
