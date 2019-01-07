
module Utils where

import Data.Char (isSpace)
import Data.List

(!!?) :: [a] -> Int -> Maybe a
xs !!? n = if (n < 0 || n > length xs) then Nothing else Just (xs !! n)

fromJustElse :: a -> Maybe a -> a
fromJustElse a = maybe a id

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

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of 
    [(x, "")] -> Just x
    _         -> Nothing

til :: Monad m => m a -> (a -> Maybe b) -> m b
til gen f = do 
    x <- gen
    case f x of
        Just x' -> return x'
        Nothing -> gen `til` f 

showAll :: Show a => [a] -> String
showAll = unwords . map show

wordlines :: String -> [[String]]
wordlines = map words . lines

unwordlines :: [[String]] -> String
unwordlines = unlines . map unwords
