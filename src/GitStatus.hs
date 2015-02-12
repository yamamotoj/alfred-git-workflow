module GitStatus where
import GitWorkflow

type GitStatus = (Char, Char, String, Maybe String)

pathStatuses :: IO (Either String [GitStatus])
pathStatuses = do
  r <- runGitProcess ["status", "-s"]
  case r of
    Left err -> return (Left err)
    Right ss -> return $Right $map readStatus $lines ss
    where readStatus (i:w:xs) = case words xs of
                                  (path:[]) -> (i,w,path,Nothing)
                                  (path1:"->":path2:[]) -> (i,w,path1,Just path2)

statusToString :: GitStatus -> String
statusToString (i,w,p,Nothing) = i:w:' ':p
statusToString (i,w,p1,Just p2) = i:w:' ':(p1 ++ " -> " ++ p2)

pathFromStatus :: GitStatus -> String
pathFromStatus (_,_,p,_) = p
