
import Utils
import Pgn.Parser
import Tree.Splay

main = do
    inp <- getContents
    case parse game inp of
        Nothing -> error "failed to parse pgn"
        Just ((gs,_),_) -> putStr . unlines . map showAll $ (concat . map peel) gs
