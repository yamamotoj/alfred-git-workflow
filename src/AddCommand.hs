module AddCommand where

import GitWorkflow
import GitCommand
import GitCommandType
import GitStatus
import Alfred
import Data.List
import Candidate
    
data AddCommand = AddCommand
instance Candidate AddCommand where
    name c  = name $commandType c
    description c = description $commandType c
    isAvailable c = isAvailable $commandType c

instance GitCommand AddCommand where
    commandType AddCommand = AddCommandType
    commandOptions  AddCommand  =
        [ (["-n", "--dry-run"], "Don’t actually add the file(s), just show if they exist and/or will be ignored.", True)
        , (["-v", "--verbose"], "Be verbose.", False)
        , (["-f", "--force"], "Allow adding otherwise ignored files.", True)
        , (["-i", "--interactive"], "", False)
        , (["-p", "--patch"], "", False)
        , (["-e", "--edit"], "", False)
        , (["-u", "--update"], "", True)
        , (["-A","--all","--no-ignore-removal"], "", True)
        , (["--no-all", "--ignore-removal"], "", True)
        , (["-N", "--intent-to-add"], "", True)
        , (["--refresh"], "", True)
        , (["--ignore-errors"], "", True)
        , (["--ignore-missing"], "", True)
        ]

    processCommand cmd args = do
      Right ss <- pathStatuses
      let paths = map (\(_,_,p,_) -> (PathCandidate p)) $filter canAdd ss
      putFeedbacks (makeFeedbacks cmd args []  paths)
        where canAdd (i,w,p,_) = (elem i " MARC?") && (elem w "MD?")


