
import Data.Tree
import Tree.Splay
import Tree.Peel
import Utils
import System.Exit

main = do
    tree <- fmap (head.unpeel.addRoot.wordlines) getContents
    putStr (splayn tree)
    exitWith (ExitFailure (width tree))


addRoot =  map (diamond:)
    where diamond = "\x25c6" 

