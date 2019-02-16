
import Data.Tree
import Tree.Splay
import Tree.Peel
import Utils

main = do
    grid <- fmap wordlines getContents
    putStr . splay . addRoot . unpeel $ grid 

addRoot = Node diamond
    where diamond = "\x25c6" 

