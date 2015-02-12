module Candidate where
import Data.List (isPrefixOf)

class Candidate a where
    name :: a -> String
    description :: a -> String
    isAvailable :: a -> Bool

lookUpCandidates :: (Candidate a) => String -> [a] -> [String] ->  Either [a] a
lookUpCandidates arg cs excludes = lookUpCandidatesAcc arg (selectCandidates cs excludes) []
    where selectCandidates cs excludes = filter (\c -> not (elem (name c)  excludes)) cs
          lookUpCandidatesAcc arg [] fs = Left fs
          lookUpCandidatesAcc arg (c:cs) fs
              | arg == (name c) = Right c
              | isPrefixOf arg (name c)   = lookUpCandidatesAcc arg cs (c:fs)
              | otherwise = lookUpCandidatesAcc arg cs fs

selectCandidates :: (Candidate a) => [a] -> [String] -> [a]
selectCandidates cs excludes = filter (\c -> not (elem (name c)  excludes)) cs                            
