
module Tree.Splay where

import Data.Tree
import Data.Char
import Utils
import Control.Applicative

import qualified Data.Foldable as Fold

splay :: Tree String -> String
splay t = unlines $ map (branchars . (uncurry zip)) $ zip unbranched (tail firstss)
    where 
    unbranched = splaylevels . levels . leafcount . stringstretch $ t
    firstss = map firsts unbranched ++ [repeat False]

depth :: Tree a -> Int
depth = length . levels

stretch :: Tree a -> Tree (Maybe a)
stretch t = stretchHelper (depth t) t

stretchHelper :: Int -> Tree a -> Tree (Maybe a)
stretchHelper depth (Node x []) = Node (Just x) $ extension (depth-1)
    where extension n = (!!n) $ iterate (\x -> [Node Nothing x]) []
stretchHelper depth (Node x ts) = Node (Just x) $ map (stretchHelper (depth-1)) ts

splaylevels :: [[(String,Int)]] -> [String]
splaylevels ls = map concat . (map . map) (expand len) $ ls
    where len = length . fst . head . head $ ls

expand :: Int -> (String,Int) -> String
expand wid (s,num) = s ++ (replicate buf ' ')  
    where buf = (num - 1) * wid

leafcount ::  Tree a -> Tree (a,Int)
leafcount (Node x []) = Node (x,1) []
leafcount (Node x ts) = Node (x,count) ts' 
    where
    ts' = map leafcount ts
    count = sum $ map numleafs ts'
    numleafs = snd . rootLabel

stringstretch :: Tree String -> Tree String
stringstretch t = fmap (makeWidth width) . fmap (fromJustElse "") . stretch $ t
    where 
    width = (+1) . Fold.maximum . fmap length $ t
    makeWidth n s = take n $ s ++ repeat ' '

firsts :: [Char] -> [Bool]
firsts [] = repeat False
firsts cs = (not . isSpace . head) cs : (map isFirst (zip cs (tail cs))) ++ repeat False
    where isFirst (x,y) = isSpace x && (not . isSpace) y

branchars :: [(Char,Bool)] -> [Char]
branchars [] = []
branchars [cb] = [branchar cb ' ']
branchars (cb:cbs) = (c:cs)
    where
    cs = branchars cbs
    c = branchar cb (head cs)
    
branchar :: (Char,Bool) -> Char -> Char
branchar (' ', False) c
    | isBranch c = bridge
branchar (' ', True) c 
    | isBranch c = tee
    | otherwise =  corner
branchar (c, _) _ = c

isBranch :: Char -> Bool
isBranch = ternary (||)
    <$> (==corner)
    <*> (==tee)
    <*> (==bridge)

corner = '\x2510'
tee = '\x252c'
bridge = '\x2500' -- create unicode/box declaration module
