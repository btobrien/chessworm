

module Tree.Peel (peel, unpeel) where 

import Data.List
import Data.Tree
import Utils

peel :: Tree a -> [[a]]
peel (Node x []) = [[x]]
peel (Node x ts) = map (x:) . concat $ map peel ts

unpeel :: Eq a => [[a]] -> [Tree a]
unpeel = map unpeel' . groupBy samenext

unpeel' :: Eq a => [[a]] -> Tree a
unpeel' ls = Node val children
    where
    val = head . head $ ls
    children = if isleaf ls
        then []
        else map unpeel' . groupBy samenext . map tail $ ls

samenext x y = head x == head y

isleaf :: Eq a => [[a]] -> Bool
isleaf = all equal . buddies
