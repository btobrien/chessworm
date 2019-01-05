
module Tree.Rose where 

import Data.List

data Tree b a = Node a [Tree b a] | Leaf b
    deriving Show

instance Functor (Tree b) where
    fmap _ (Leaf x) = Leaf x
    fmap f (Node x ns) = Node (f x) (map (fmap f) ns)

peel :: Tree b a -> [[a]]
peel (Leaf _) = [[]]  
peel (Node x ts) = map (x:) . concat $ map peel ts

pick :: Tree b a -> [b]
pick (Leaf x) = [x]
pick (Node _ ts) = concat . map pick $ ts

unpeel :: Eq a => b -> [[a]] -> Tree b a
unpeel r [[]] = Leaf r
unpeel r ls = Node val children
    where
    val = head . head $ ls
    children = map (unpeel r) . groupBy samenext . map tail $ ls
samenext x y = head x == head y

mainleaf :: Tree b a -> b
mainleaf (Leaf x) = x
mainleaf (Node _ (t:_)) = mainleaf t

