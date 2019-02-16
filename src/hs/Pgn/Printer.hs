
module Pgn.Printer where

import System.IO
import Data.List
import Control.Monad
import Data.Tree

import Pgn.Parser

-- collapse space functions?

showPrecomment "" = "" 
showPrecomment p = "{" ++ p ++ "} "

showComment "" = "" 
showComment c = " {" ++ c ++ "}"

showIntGlyph 0 = ""
showIntGlyph n = " $" ++ show n

showMove numstr (Node (Move p m g c) _) = showPrecomment p ++ numstr ++ m ++ showIntGlyph g ++ showComment c

showNum n = show (1 + (n `div` 2)) ++ dot ++ " "
    where dot = if even n then "." else "..."

putSubTree :: Int -> MoveTree -> IO ()
putSubTree n m = do
    putStr "(" 
    putStr $ showMove (showNum n) m 
    putStr " " 
    putSubTrees (n+1) m
    putStr "\b\b) "  -- backspaces??

putSubTrees :: Int -> MoveTree -> IO ()
putSubTrees _ (Node _ []) = return ()
putSubTrees n (Node _ (t:ts)) = do
    when (even n) (putStr $ showNum n)
    putStr $ showMove "" t 
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
    putStrLn "\"]"

putGame (trees,tags) = do
    putStrLn ""
    mapM_ putTag tags 
    putStrLn "" 
    putTrees trees
    print $ result (head trees)

