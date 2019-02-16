
import Data.Char
import System.IO
import System.Environment
import Data.List
import Control.Monad

import Tree.Climb

type State = (Tree String, Location)

main = do 
    hSetBuffering stdout NoBuffering
    inp <- getLine
    tree <- getTree
    mapM_ putState $ treeScan (tree,start) (lines inp)

getTree :: IO (Tree String)
getTree = do
    args <- getArgs
    if null args then return nulltree else getFileTree (head args)

getFileTree :: String -> IO (Tree String)
getFileTree fname = do
    file <- readFile fname
    let ls = lines file
    return $ if null ls then nulltree else map words ls

treeScan :: State -> [String] -> [State]
treeScan = scanl' nextState
    where nextState = flip readcmd

putState :: State -> IO ()
putState s@(t,(h,d)) = do 
    putStr.currentVal $ s
    putStr " "
    putStr.show $ h
    putStr " "
    putStr.show $ d
    putStr " "
    putStrLn . showTree $ t

showTree :: Tree String -> String
showTree = intercalate "]," . map (intercalate ",")

currentVal :: State -> String
currentVal (_,(_,-1)) = "-"
currentVal s = uncurry get s
