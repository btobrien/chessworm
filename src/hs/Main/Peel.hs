
import Data.List

import Monad.Parser
import Tree.Rose
import Pgn.Parser
import Pgn.Glyph as Glyph

main = do
    inp <- getContents
    case parse game inp of
        Nothing -> error "failed to parse pgn"
        Just ((gs,_),_) -> putStr . unlines . map showLine $ (concat . map peel) gs

showLine :: [Move] -> String
showLine = intercalate " " . map showNode

showNode :: Move -> String
showNode (Move _ x g _) = x ++ Glyph.toString g
