
module Utils.Control where

import Data.Char (isSpace)

(!!?) :: [a] -> Int -> Maybe a
xs !!? n = if (n < 0 || n > length xs) then Nothing else Just (xs !! n)

fromJustElse :: a -> Maybe a -> a
fromJustElse a ma = case ma of
    Nothing -> a
    Just x -> x

equal :: Eq a => (a,a) -> Bool
equal = uncurry (==)

pair :: a -> b -> (a,b)
pair a b = (a,b)

limit :: Eq a => (a -> a) -> a -> a
limit f x = if (x == x') then x else limit f x'
    where x' = f x

while :: Eq a => (a -> Maybe a) -> a -> a
while f x = case f x of
        Nothing -> x
        Just x' -> while f x'

replace :: Int -> a -> [a] -> [a]
replace n x xs = take n xs ++ [x] ++ drop (n+1) xs

trim :: String -> String
trim = f . f
    where f = reverse . dropWhile isSpace
