
import System.Environment
import Control.Applicative
import qualified Data.Map as Map

import Monad.Parser
import Pgn.Parser

main = do
    inp <- getContents
    tagname <- getTagname
    let tags = parseTags inp
    case Map.lookup tagname tags of
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
