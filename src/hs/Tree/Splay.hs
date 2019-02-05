
module Tree.Splay where

import Data.Tree
import Data.Char
import Utils

numleafs :: Tree (a,Int) -> Int
numleafs (Node (_,n) _) = n

leafcount ::  Tree a -> Tree (a,Int)
leafcount (Node x []) = Node (x,1) []
leafcount (Node x ts) = Node (x,count) ts' 
    where
    ts' = map leafcount ts
    count = sum $ map numleafs ts'

splay :: Tree String -> String
splay t = unlines $ map (branchars . ziptran) (zip unbranched (tail firstss))
    where 
    unbranched = splaylevels . levels . leafcount $ t
    firstss = map firsts unbranched ++ [repeat False]

ziptran :: ([a],[b]) -> [(a,b)]
ziptran (as,bs) = zip as bs 

splaylevels :: [[(String,Int)]] -> [String]
splaylevels ls = (map concat) . ((map . map) (expand len)) $ ls
    where len = length . fst . head . head $ ls

expand :: Int -> (String,Int) -> String
expand wid (s,num) = s ++ (replicate buf ' ')  
    where buf = (num - 1) * wid

stretch' :: Int -> Tree a -> Tree (Maybe a)
stretch' depth (Node x []) = Node (Just x) (extension n)
stretch' depth (Node x ts) = Node (Just x) $ map (stretch (depth-1)) ts

extension :: Int -> [Tree (Maybe a)]
extension n = (!!n) $ iterate (\x -> [Node Nothing x]) []

t = Node "1a " [
        Node "1b " [
            Node "1c " [],
            Node "2c " [
                Node "1d " [],
                Node "2d " []],
            Node "3c " []],
        Node "2b " [
            Node "1c " [],
            Node "2c " []],
        Node "3b " []]

firsts :: [Char] -> [Bool]
firsts cs = True : (map isFirst (zip cs (tail cs))) ++ repeat False

isFirst :: (Char,Char) -> Bool
isFirst (x,y) = isSpace x && (not . isSpace) y

branchars :: [(Char,Bool)] -> [Char]
branchars [] = []
branchars [cb] = [branchar cb ' ']
branchars (cb:cbs) = (c:cs)
    where
    cs = branchars cbs
    c = branchar cb (head cs)
    
corner = '\x2510'
tee = '\x252c'
bridge = '\x2500'

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

cb = zip "1     2   3" [True,False,True,False,True,False,True,False,True,False,False]
