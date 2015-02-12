module GitCommand where

import Alfred
import Data.List
import GitWorkflow
import GitStatus
import Candidate
import GitCommandType
import CommandOption
                                        
data TreeIsh = String deriving Show
data PathCandidate = PathCandidate String deriving Show
instance Candidate PathCandidate where
    name (PathCandidate n) = n
    description _ = ""
    isAvailable _ = True
data CommandArg = OptionArg CommandOptionCandidate
                | PartialOptionArg [CommandOptionCandidate]
                | TreeIshArg TreeIsh
                | PathArg PathCandidate
                | PartialPathArg [PathCandidate]
                | ILLigalArg
                  deriving Show

class (Candidate c) =>GitCommand c where
    commandType :: c -> GitCommandType
    commandOptions :: c -> [CommandOption]
    processCommand :: c  -> [String] ->IO()
    processCommand cmd args = putFeedbacks [commandFeedback cmd args]
                              
readArg :: (GitCommand a) => a -> String -> [String] -> [PathCandidate] -> CommandArg
readArg cmd ('-':'-':xs) args _  =
    case lookUpCandidates ('-':xs) (longOptionCandidates (commandOptions cmd)) args of
      Right x -> OptionArg x
      Left [] -> ILLigalArg
      Left xs -> PartialOptionArg xs
readArg cmd ('-':xs) args _ =
    case lookUpCandidates ('-':xs) (shortOptionCandidates (commandOptions cmd)) args of
      Right x -> OptionArg x
      Left [] -> ILLigalArg
      Left xs -> PartialOptionArg xs
readArg _ xs args paths =
    case lookUpCandidates xs paths args of
      Right x -> PathArg x
      Left [] -> ILLigalArg
      Left xs -> PartialPathArg xs
                              

makeFeedbacks :: (GitCommand a) => a -> [String] -> [String] -> [PathCandidate] -> [Feedback]
makeFeedbacks cmd [] accArgs paths = map (pathCandidateFeedback cmd accArgs) $selectCandidates paths accArgs
makeFeedbacks cmd (a:[]) accArgs paths = 
    case readArg cmd a accArgs paths of
      OptionArg o          -> makeFeedbacks cmd [] ((name o):accArgs) paths
      PartialOptionArg cs  -> map (optionCandidateFeedback cmd accArgs) cs
      PathArg path         -> (commandFeedback cmd ((name path):accArgs)):makeFeedbacks cmd [] ((name path):accArgs) paths
      PartialPathArg paths -> map (pathCandidateFeedback cmd accArgs) paths
makeFeedbacks cmd (a:as) accArgs paths =
    case readArg cmd a accArgs paths of
      OptionArg o  -> makeFeedbacks cmd as ((name o):accArgs) paths
      PathArg path -> makeFeedbacks cmd as ((name path):accArgs) paths
      otherwise    -> []
                  
pathCandidateFeedback :: (GitCommand a) => a -> [String] -> PathCandidate -> Feedback
pathCandidateFeedback cmd args (PathCandidate path) =
    let arg = (name cmd) ++ " " ++ (unwords $reverse args) ++ " " ++ path
    in Feedback { uid = arg
                , arg = ""
                , valid = False
                , autocomplete = arg
                , title = path
                , subtitles = [Normal ("git "  ++ arg)]
                , icon = Nothing
                }
commandFeedback :: (GitCommand a) => a -> [String] -> Feedback
commandFeedback cmd args = let as= (name (commandType cmd)) ++ " " ++ (unwords $reverse args)
                           in Feedback { uid = as
                                       , arg = (name (commandType cmd)) ++ " -v  " ++ (unwords $reverse args)
                                       , valid = True
                                       , autocomplete = as
                                       , title = "git " ++ as
                                       , subtitles = [Normal (description (commandType cmd))]
                                       , icon = Nothing
                                       }

optionCandidateFeedback :: (GitCommand a) => a -> [String] -> CommandOptionCandidate -> Feedback
optionCandidateFeedback cmd args c  =
    let CommandOptionCandidate n (ops,_,_) = c
        otherOptions = filter (n/=) ops in
    if isAvailable c then
        Feedback { uid = name c
                 , arg = ""
                 , valid = False
                 , autocomplete = (name cmd) ++ " " ++ (unwords $reverse args) ++ " " ++ (name c)
                 , title = name c ++ " ( " ++ (unwords otherOptions) ++ " )"
                 , subtitles = [Normal (description c)]
                 , icon = Nothing
                 }
    else
        Feedback { uid = name c
                 , arg = ""
                 , valid = False
                 , autocomplete = (name cmd) ++ " " ++ (unwords $reverse args)
                 , title = name c ++ " ( " ++ (unwords otherOptions) ++ " ) " ++ ":Not available"
                 , subtitles = [Normal "This option is not supported"]
                 , icon = Nothing
                 }
