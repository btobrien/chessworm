
import Data.List

import Tree.Rose
import Pgn.Parser
import Pgn.Printer

readmove m = Move "" m 0 ""

main = do
    inp <- getContents
    let tree = map (map readmove) $ map words $ lines inp
    putTrees . map (unpeel Unknown) . groupBy samenext $ tree
    putStrLn "\b*"
