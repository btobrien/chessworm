
import Data.List
import System.Environment
import System.Exit
import Utils

import Pgn.Parser (parse, game, move)
import Tree.Peel (peel)

main = do
    pgn <- getPgn
    let mainline = parseMainline pgn
    line <- fmap words getLine
    if line `isPrefixOf` mainline then 
        putWords $ nextmove mainline line
    else
        putWords (lcp mainline line) >> exitFailure
    
putWords = putStrLn . unwords

nextmove :: Eq a => [a] -> [a] -> [a]
nextmove main pre = take (length pre + 1) main

lcp xs ys = map fst . takeWhile equal $ zip xs ys

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
        Just ((g:_,_),_) -> map move . head . peel $ g

