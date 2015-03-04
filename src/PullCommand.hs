module PullCommand where

import qualified  GitStatus as Status
import qualified  GitRemote as Remote
import Candidate
import GitCommand
import GitCommandType
import Alfred

data PullCommand = PullCommand

instance Candidate PullCommand where
    name c  = name $commandType c
    description c = description $commandType c
    isAvailable c = isAvailable $commandType c

instance GitCommand PullCommand where
    commandType PullCommand = PullCommandType
    commandOptions PullCommand  = []

    processCommand cmd args = do
      putFeedbacks $commandFeedback cmd args:[]

