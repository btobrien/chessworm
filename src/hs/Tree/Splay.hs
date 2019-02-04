
module Tree.Willow where

import Data.Tree


numleafs :: Tree (a,Int) -> Int
numleafs (Node (_,n) _) = n

leafcount ::  Tree a -> Tree (a,Int)
leafcount (Node x []) = Node (x,1) []
leafcount (Node x ts) = Node (x,count) ts' 
    where
    ts' = map leafcount ts
    count = sum $ map numleafs ts'


willowshow :: Tree String -> String
willowshow = willowshow' . levels . leafcount

willowshow' :: [[(String,Int)]] -> String
willowshow' t = unlines . (map unwords) $ (map . map) (expand 1) t

expand :: Int -> (String,Int) -> String
expand wid (s,num) = s ++ (replicate buf ' ')  
    where buf = (num * wid) - 1
