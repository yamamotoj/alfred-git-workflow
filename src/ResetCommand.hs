module ResetCommand
    where


import Candidate
import GitCommand
import GitCommandType
import GitStatus
import Alfred
import CommandOption

data ResetCommand = ResetCommand
instance Candidate ResetCommand where
    name c  = name $commandType c
    description c = description $commandType c
    isAvailable c = isAvailable $commandType c

instance GitCommand ResetCommand where
    commandType cmdd = AddCommandType
    commandOptions  cmd = [ (["-q"], "", False)
                          , (["-p", "--patch"], "", False)
                          , (["--sort, --mixed", "--hard", "--merge", "--keep"], "", True)
                          ]
    processCommand cmd args = do
      Right ss <- pathStatuses
      let paths = map (\(_,_,p,_) -> (PathCandidate p)) $filter canReset ss
      putFeedbacks (makeFeedbacks cmd args []  paths)
        where canReset (i,_,_,_) = (elem i "MARC")

data GitResetMode = GitIndexReset
                  | GitInteractiveIndexReset
                  | GitCommitReset

data TreeIsh = HEAD
    
makeResetFeedbacks :: (GitCommand a) => a -> [String] -> [String] -> [PathCandidate]  -> [Feedback]
makeResetFeedbacks cmd (a:as) accArgs paths =
    case readArg cmd a accArgs paths of
      OptionArg (CommandOptionCandidate "--sort" _) -> makeCommitResetFeedbacks cmd as accArgs paths
      OptionArg (CommandOptionCandidate "--mixed" _) -> makeCommitResetFeedbacks cmd as accArgs paths
      OptionArg (CommandOptionCandidate "--hard" _) -> makeCommitResetFeedbacks cmd as accArgs paths
      OptionArg (CommandOptionCandidate "--merge" _) -> makeCommitResetFeedbacks cmd as accArgs paths
      OptionArg (CommandOptionCandidate "--keep" _) -> makeCommitResetFeedbacks cmd as accArgs paths
      PartialOptionArg arg -> map (optionCandidateFeedback cmd accArgs) arg
      PathArg path -> makeFeedbacks cmd as accArgs paths
      PartialPathArg path -> map (pathCandidateFeedback cmd accArgs) path
                             
makeCommitResetFeedbacks :: (GitCommand a) => a -> [String] -> [String] -> [PathCandidate] -> [Feedback]
makeCommitResetFeedbacks cmd (a:as) accArgs commits =
    case readArg cmd a accArgs commits of
      PartialOptionArg arg -> map (optionCandidateFeedback cmd accArgs) arg
      PartialPathArg cs -> map (pathCandidateFeedback cmd accArgs) cs
      PathArg commit -> (commandFeedback cmd ((name commit):accArgs)):makeFeedbacks cmd [] ((name commit):accArgs) commits
      
