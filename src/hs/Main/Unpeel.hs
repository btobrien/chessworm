
import Data.List
import Utils

import Tree.Peel
import Pgn.Printer

main = do
    inp <- getContents
    let tree = (map.map) read . wordlines $ inp
    putTrees $ unpeel undefined tree
    putStrLn "*" -- have arg define result
