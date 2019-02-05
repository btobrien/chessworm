
import Data.Tree
import Tree.Splay
import Tree.Peel
import Utils

main = do
    grid <- fmap wordlines getContents
    let width = maximum . (map maximum) . ((map . map) length) $ grid
    putStr . splay width . addRoot . unpeel $ grid 

addRoot ts = Node diamond ts
    where diamond = "\x25c6" 

