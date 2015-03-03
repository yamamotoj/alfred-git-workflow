module GitRemote
    where

import qualified GitWorkflow as Git
import Candidate
import Control.Monad
import GitCommand      
type GitRemote = String
                        
list :: IO [GitRemote]
list = do
  r <- Git.runGitProcess ["remote"]
  case r of
    Left _ -> return []
    Right r -> return  $lines r

candidates :: IO [PathCandidate]
candidates = liftM (map PathCandidate) list
