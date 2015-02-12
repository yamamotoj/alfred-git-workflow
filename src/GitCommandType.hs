module GitCommandType where
import Candidate
data GitCommandType = AddCommandType
                | BisectCommandType
                | BranchCommandType
                | CheckoutCommandType
                | CloneCommandType
                | CommitCommandType
                | DiffCommandType
                | FetchCommandType
                | GrepCommandType
                | InitCommandType
                | LogCommandType
                | MergeCommandType
                | MvCommandType
                | PullCommandType
                | PushCommandType
                | RebaseCommandType
                | ResetCommandType
                | RmCommandType
                | ShowCommandType
                | StatusCommandType
                | TagCommandType
                  deriving Show

instance Candidate GitCommandType where                    
    name AddCommandType = "add"
    name BisectCommandType = "bisect"
    name BranchCommandType = "branch"
    name CheckoutCommandType = "checkout"
    name CloneCommandType = "clone"
    name CommitCommandType = "commit"
    name DiffCommandType = "diff"
    name FetchCommandType = "fetch"
    name GrepCommandType = "grep"
    name InitCommandType = "init"
    name LogCommandType = "log"
    name MergeCommandType = "merge"
    name MvCommandType = "mv"
    name PullCommandType = "pull"
    name PushCommandType = "push"
    name RebaseCommandType = "rebase"
    name ResetCommandType = "reset"
    name RmCommandType = "rm"
    name ShowCommandType = "show"
    name StatusCommandType = "status"
    name TagCommandType = "tag"
    description AddCommandType = "Add file contents to the index"
    description BisectCommandType = "Find by binary search the change that introduced a bug"
    description BranchCommandType = "List, create, or delete branches"
    description CheckoutCommandType = "Checkout a branch or paths to the working tree"
    description CloneCommandType = "Clone a repository into a new directory"
    description CommitCommandType = "Record changes to the repository"
    description DiffCommandType = "Show changes between commits, commit and working tree, etc"
    description FetchCommandType = "Download objects and refs from another repository"
    description GrepCommandType = "Print lines matching a pattern"
    description InitCommandType = "Create an empty Git repository or reinitialize an existing one"
    description LogCommandType = "Show commit logs"
    description MergeCommandType = "Join two or more development histories together"
    description MvCommandType = "Move or rename a file, a directory, or a symlink"
    description PullCommandType = "Fetch from and integrate with another repository or a local branch"
    description PushCommandType = "Update remote refs along with associated objects"
    description RebaseCommandType = "Forward-port local commits to the updated upstream head"
    description ResetCommandType = "Reset current HEAD to the specified state"
    description RmCommandType = "Remove files from the working tree and from the index"
    description ShowCommandType = "Show various types of objects"
    description StatusCommandType = "Show the working tree status"
    description TagCommandType = "Create, list, delete or verify a tag object signed with GPG"
    isAvailable AddCommandType = False
    isAvailable BisectCommandType = False
    isAvailable BranchCommandType = False
    isAvailable CheckoutCommandType = False
    isAvailable CloneCommandType = False
    isAvailable CommitCommandType = False
    isAvailable DiffCommandType = False
    isAvailable FetchCommandType = False
    isAvailable GrepCommandType = False
    isAvailable InitCommandType = False
    isAvailable LogCommandType = False
    isAvailable MergeCommandType = False
    isAvailable MvCommandType = False
    isAvailable PullCommandType = False
    isAvailable PushCommandType = False
    isAvailable RebaseCommandType = False
    isAvailable ResetCommandType = False
    isAvailable RmCommandType = False
    isAvailable ShowCommandType = False
    isAvailable StatusCommandType = False
    isAvailable TagCommandType = False


commandTypes :: [GitCommandType]
commandTypes = [ AddCommandType
               , BisectCommandType
               , BranchCommandType
               , CheckoutCommandType
               , CloneCommandType
               , CommitCommandType
               , DiffCommandType
               , FetchCommandType
               , GrepCommandType
               , InitCommandType
               , LogCommandType
               , MergeCommandType
               , MvCommandType
               , PullCommandType
               , PushCommandType
               , RebaseCommandType
               , ResetCommandType
               , RmCommandType
               , ShowCommandType
               , StatusCommandType
               , TagCommandType
               ]

