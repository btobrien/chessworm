module Peel where 
import PgnParser

peel :: Tree b a -> [[a]]
peel (Leaf _) = [[]]  
peel (Node x ts) = map (x:) . concat $ map peel ts

pick :: Tree b a -> [b]
pick (Leaf x) = [x]
pick (Node _ ts) = concat . map pick $ ts
