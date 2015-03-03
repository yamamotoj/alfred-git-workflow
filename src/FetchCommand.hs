module FetchCommand where

import qualified  GitStatus as Status
import qualified  GitRemote as Remote
import Candidate
import GitCommand
import GitCommandType
import Alfred

data FetchCommand = FetchCommand

instance Candidate FetchCommand where
    name c  = name $commandType c
    description c = description $commandType c
    isAvailable c = isAvailable $commandType c

instance GitCommand FetchCommand where
    commandType FetchCommand = FetchCommandType
    commandOptions  FetchCommand  =
        [
         (["--all"], "", True)
        ]

    processCommand cmd args = do
      repos <- Remote.candidates
      putFeedbacks (makeFeedbacks cmd args []  repos)

    
