
import System.Environment
import Control.Applicative
import qualified Data.Map as Map

import Pgn.Parser

main = do
    inp <- getContents
    tagname <- getTagname
    case Map.lookup tagname (parseTags inp) of
        Nothing -> error (notfound tagname)
        Just v -> putStrLn v

getTagname = do
    args <- getArgs
    if null args then error noarg else return (head args)

parseTags inp = case parse (many tag) inp of
        Nothing -> error "no tags found"
        Just (tags,_) -> Map.fromList tags

noarg = "no tagname provided as argument"
notfound tagname = "tag=" ++ tagname ++ " not found"
