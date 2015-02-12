module CommandOption where
import Candidate
import Data.List
    
type CommandOption = ([String], String, Bool)
data CommandOptionCandidate = CommandOptionCandidate String CommandOption deriving Show
instance Eq CommandOptionCandidate where
    (==) a b = (name a) == (name b)

instance Ord CommandOptionCandidate where
    compare a b =
        if isAvailable a then
            if isAvailable b
            then compare (name a) (name b)
            else GT
        else if isAvailable b
             then LT
             else compare (name a) (name b)

instance Candidate CommandOptionCandidate where
    name (CommandOptionCandidate n _ ) = n
    description (CommandOptionCandidate _ (_,d,_)) = d
    isAvailable (CommandOptionCandidate _ (_,_,b)) = b

optionCandidates :: [CommandOption] -> [CommandOptionCandidate]
optionCandidates ops = foldl (\cs op -> cs ++ (toCandidates op)) [] ops
    where toCandidate op name =  CommandOptionCandidate name op
          toCandidates ( names, desc, b) = map (toCandidate (names, desc, b)) names

shortOptionCandidates :: [CommandOption] -> [CommandOptionCandidate]
shortOptionCandidates = sort.filter (\c -> case name c of
                                        ('-':'-':ss) -> False
                                        ('-':xs) -> True
                                        otherwise -> False
                                        ) . optionCandidates

longOptionCandidates :: [CommandOption] -> [CommandOptionCandidate]
longOptionCandidates = sort.filter (\c -> case name c of
                                        ('-':'-':ss) -> True
                                        otherwise -> False
                                        ) . optionCandidates

