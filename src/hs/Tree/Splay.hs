
module Tree.Splay (splayn, width) where

import Data.Tree
import Data.Char
import Utils
import Control.Applicative
import Data.Maybe

import qualified Data.Foldable as Fold

splay :: Tree String -> String
splay = unlines . splays

splayn :: Tree String -> String
splayn t = unlines . zipWith addNumber [0..] . splays $ t 

addNumber :: Int -> String -> String
addNumber 0 str = (++str) $ replicate 5 ' '
addNumber n str =  (++str) . take 5 $ show n ++ repeat ' '


splays :: Tree String -> [String]
splays t = zipWith addbranches unbranched firsts
    where 
    unbranched = expandlevels . levels . leafcount . stringstretch $ t
    expandlevels = map $ concatMap expand
    firsts = tail $ map firstmap unbranched ++ [repeat False]

depth :: Tree a -> Int
depth = length . levels

stretch :: Tree a -> Tree (Maybe a)
stretch t = stretch' (depth t) t

stretch' :: Int -> Tree a -> Tree (Maybe a)
stretch' depth (Node x []) = Node (Just x) $ extension (depth-1)
    where extension n = (!!n) $ iterate (\x -> [Node Nothing x]) []
stretch' depth (Node x ts) = Node (Just x) $ map (stretch' (depth-1)) ts

stringstretch :: Tree String -> Tree String
stringstretch t = fmap (makeWidth . fromMaybe "") . stretch $ t
    where 
    makeWidth str = take (width t) (str ++ repeat ' ')

width :: Tree String -> Int
width = (+1) . Fold.maximum . fmap length

leafcount ::  Tree a -> Tree (a,Int)
leafcount (Node x []) = Node (x,1) []
leafcount (Node x ts) = Node (x,count) ts' 
    where
    ts' = map leafcount ts
    count = sum $ map numleafs ts'
    numleafs = snd . rootLabel

expand :: (String,Int) -> String
expand (s,num) = s ++ replicate buf ' '
    where 
    buf = (num - 1) * wid
    wid = length s

firstmap :: [Char] -> [Bool]
firstmap [] = repeat False
firstmap cs = (b:bs) ++ repeat False
    where 
    b = not . isSpace . head $ cs
    bs = map isFirst (buddies cs)
    isFirst (x,y) = isSpace x && (not . isSpace) y

addbranches :: [Char] -> [Bool] -> [Char]
addbranches cs = addbranches' . zip cs

addbranches' :: [(Char,Bool)] -> String
addbranches' [] = []
addbranches' [cb] = [branchar cb ' ']
addbranches' (cb:cbs) = c:cs
    where
    cs = addbranches' cbs
    c = branchar cb (head cs)
    
branchar :: (Char,Bool) -> Char -> Char
branchar (' ', False) c
    | isBranch c = bridge
branchar (' ', True) c 
    | isBranch c = tee
    | otherwise =  corner
branchar (c',_) _ = c'

isBranch :: Char -> Bool
isBranch = ternary (||)
    <$> (==corner)
    <*> (==tee)
    <*> (==bridge)

corner = '\x2510'
tee = '\x252c'
bridge = '\x2500' -- create unicode/box declaration module
