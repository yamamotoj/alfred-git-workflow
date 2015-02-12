


data ResetMode = NormalReset
               | SoftReset
               | HardReset
               | MixedReset
               | MergeReset
               | KeepReset


data TreeIsh = HEAD


             
processCommand ResetCommand args = do
  where getMode [] = "--"
        getMode ("--":xs) = "--"
        getMode ("--soft":_)
        getTreeish ("":)


