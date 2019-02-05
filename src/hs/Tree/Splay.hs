
module Tree.Splay where

import Data.Tree
import Data.Char
import Utils
import Control.Applicative

splay :: Int -> Tree String -> String
splay width t = unlines $ map (branchars . (uncurry zip)) (zip unbranched (tail firstss))
    where 
    unbranched = splaylevels . levels . leafcount . (stringstretch (width+1)) $ t
    firstss = map firsts unbranched ++ [repeat False]

splaylevels :: [[(String,Int)]] -> [String]
splaylevels ls = (map concat) . ((map . map) (expand len)) $ ls
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

numleafs :: Tree (a,Int) -> Int
numleafs (Node (_,n) _) = n

stringstretch :: Int -> Tree String -> Tree String
stringstretch width = (fmap (makeWidth width)) . (fmap (fromJustElse "")) . stretch

makeWidth :: Int -> String -> String
makeWidth n s = take n $ s ++ repeat ' '

stretch :: Tree a -> Tree (Maybe a)
stretch t = stretch' d t
    where d = depth t

stretch' :: Int -> Tree a -> Tree (Maybe a)
stretch' depth (Node x []) = Node (Just x) $ extension (depth-1)
stretch' depth (Node x ts) = Node (Just x) $ map (stretch' (depth-1)) ts

depth :: Tree a -> Int
depth = length . levels

extension :: Int -> [Tree (Maybe a)]
extension n = (!!n) $ iterate (\x -> [Node Nothing x]) []

t = Node "1a" [
        Node "1b" [
            Node "1c" [],
            Node "2c" [
                Node "1d" [],
                Node "2d" []],
            Node "3c" []],
        Node "2b" [
            Node "1c" [],
            Node "2c" [
                Node "1d" [
                    Node "1e" [],
                    Node "2e" [
                        Node "1f" []]]]],
        Node "3b" []]

firsts :: [Char] -> [Bool]
firsts [] = repeat False
firsts cs = (not . isSpace . head) cs : (map isFirst (zip cs (tail cs))) ++ repeat False

isFirst :: (Char,Char) -> Bool
isFirst (x,y) = isSpace x && (not . isSpace) y

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
bridge = '\x2500'

