import Peel
import PgnParser
import Parser
import Data.List
import System.Environment
import System.Exit

main = do
    pgn <- getPgn
    let mainline = parseMainline pgn
    line <- fmap words getLine
    if line `isPrefixOf` mainline then 
        putLine $ nextmove mainline line
    else
        putLine (lcp mainline line) >> exitFailure
    
nextmove :: Eq a => [a] -> [a] -> [a]
nextmove main pre = take (length pre + 1) main

lcp xs ys = map fst . takeWhile (\(x,y) -> x==y) $ zip xs ys

movestr (Move _ x _ _) = x

getPgn :: IO String
getPgn = do
    fname <- getFileName
    readFile fname

getFileName :: IO String
getFileName = do
    args <- getArgs
    if null args then error "pgn file argument not provided" else return (head args)

parseMainline :: String -> [String]
parseMainline pgn = case parse game pgn of
        Nothing -> error "failed to parse pgn"
        Just ((g:_,_),_) -> map movestr . head . peel $ g

putLine = putStrLn . intercalate " "
