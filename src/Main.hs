import Alfred
import Candidate
import GitWorkflow
import GitDirHistory 
import GitCommandType
import GitCommand
import AddCommand
import FetchCommand
import ResetCommand
import PullCommand
import Control.Applicative
import Control.Monad
import Control.Exception
import System.IO
import System.Environment (getArgs)
import System.Process (runInteractiveCommand, waitForProcess, readProcess)
import System.Posix.Directory (changeWorkingDirectory)
import System.Directory (doesDirectoryExist)
import Data.List
    
main = do
  xs <- getArgs
  case xs of
    [] -> run []
    arg:_ -> run $words arg

run:: [String] -> IO()

run [] = do
  r <- currentBranch
  case r of
    Right out -> do
           Just dir <- getCurrent
           putFeedbacks [(messageFeedback out dir)]
    Left err -> do
           r <- getCurrent
           case r of
             Nothing -> putFeedbacks [(messageFeedback "Enter: cd /your/working/dir " err)]
             Just dir -> putFeedbacks [(messageFeedback "This is not git directory " dir)]
           
run ("cd":[]) = cd []
run ("cd":path:_) = cd path
                                
run ("checkout":[]) = checkout []
run ("checkour":str:_) = checkout str

run (cmd:args) = do
  wd <- getCurrent
  case wd of
    Nothing -> cd cmd
    Just _ -> case lookUpCandidates cmd commandTypes [] of
                Right AddCommandType -> processCommand AddCommand args
                Right ResetCommandType -> processCommand ResetCommand args
                Right FetchCommandType -> processCommand FetchCommand args
                Right PullCommandType -> processCommand PullCommand args
                Left types -> do
                   putFeedbacks $map commandAutocompletes types

cd:: String -> IO()
cd path = do
  hs <- getHistory
  feedbacks <- mapM path2Feedback $ filter (isInfixOf path) hs
  f <- path2Feedback path
  putFeedbacks (f:feedbacks)
  return ()
    where path2Feedback p = do
            b <- doesDirectoryExist　p
            if b then do
                   status <- statusFromPath p
                   return $simpleFeedback2 ("cd " ++ p) status
            else return $simpleFeedback2 ("mkdir " ++ p) "Create directory"

checkout::String -> IO()
checkout s = do
  r <- runGitProcess ["branch","-a"]
  case r of
    Right out -> putFeedbacks $map branch2Feedback $filter (isInfixOf s) $lines out
    Left err -> putFeedbacks [messageFeedback err []]
    where branch2Feedback l = simpleFeedback ("checkout " ++ l)

statusFromPath::String -> IO String
statusFromPath path = do
  b <- doesDirectoryExist path
  if b then do
         r <- currentBranchInWorkingDirectory path
         case r of
           Right out -> return out
           Left err -> return err
  else return "Not Directory"

commandAutocompletes :: GitCommandType -> Feedback
commandAutocompletes cmd = Feedback { uid = name cmd
                                    , arg = ""
                                    , valid = False
                                    , autocomplete = name cmd
                                    , title = "git " ++ (name cmd)
                                    , subtitles = [Normal (description cmd)]
                                    , icon = Nothing
                                    }

