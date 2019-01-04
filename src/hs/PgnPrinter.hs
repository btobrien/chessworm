
module PgnPrinter where

import Parser
import PgnParser
import System.IO
import Data.List
import Control.Monad

main = do
    inp <- getContents
    parseGames inp

parseGames inp = do
    case parse game inp of
        Nothing -> return ()
        Just (g,[]) -> putGame g
        Just (g,inp') -> putGame g >> parseGames inp' 

-- collapse space functions


showPrecomment "" = "" 
showPrecomment p = "{" ++ p ++ "} "

showComment "" = "" 
showComment c = " {" ++ c ++ "}"

showIntGlyph 0 = ""
showIntGlyph n = " $" ++ show n

showMove _ (Leaf _) = ""
showMove numstr (Node (Move p m g c) _) = showPrecomment p ++ numstr ++ m ++ showIntGlyph g ++ showComment c

showNum n = show (1 + (n `div` 2)) ++ dot ++ " "
    where dot = if even n then "." else "..."

isNode (Node _ _) = True
isNode _ = False

putSubTree :: Int -> MoveTree -> IO ()
putSubTree n m = do
    putStr "(" 
    putStr $ showMove (showNum n) m 
    putStr " " 
    putSubTrees (n+1) m
    putStr "\b\b) "

putSubTrees :: Int -> MoveTree -> IO ()
putSubTrees _ (Leaf _) = return ()
putSubTrees n (Node _ (t:ts)) = do
    when (even n && isNode t) (putStr $ showNum n)
    putStr $ showMove [] t 
    putStr " " 
    mapM_ (putSubTree n) ts 
    let n' = n+1
    when (odd n' && (not.null) ts) (putStr $ showNum n')
    putSubTrees n' t

putTrees ts = putSubTrees 0 (Node nullmove ts)

putTag (n,v) = do 
    putStr "[" 
    putStr n 
    putStr " \"" 
    putStr v 
    putStr "\"]"
    putStr "\n"

putGame (trees,tags) = do
    putStr "\n"
    mapM_ putTag tags 
    putStr "\n" 
    putTrees trees
    putStr . show $ result (head trees)
    putStr "\n"

