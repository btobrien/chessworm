
module Utils where

import Data.Char (isSpace)
import Data.List
import Data.Maybe
import Control.Applicative

(!!?) :: [a] -> Int -> Maybe a
xs !!? n = if n < 0 || n > length xs then Nothing else Just (xs !! n)

showJust :: Show a => Maybe a -> String
showJust ma = case ma of
    Nothing -> ""
    Just a -> show a

equal :: Eq a => (a,a) -> Bool
equal = uncurry (==)

squared :: Num a => a -> a
squared a = a * a

pair :: a -> b -> (a,b)
pair a b = (a,b)

buddies :: [a] -> [(a,a)]
buddies xs = zip xs (tail xs)

limit :: Eq a => (a -> a) -> a -> a
limit f x = if x == x' then x else limit f x'
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

diffOn :: Enum b => (a -> b) -> a -> a -> Int
diffOn f x y = fromEnum (f y) - fromEnum (f x)

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

everyone :: Enum a => [a]
everyone = enumFrom . toEnum $ 0

showAll :: Show a => [a] -> String
showAll = unwords . map show

wordlines :: String -> [[String]]
wordlines = map words . lines

unwordlines :: [[String]] -> String
unwordlines = unlines . map unwords

infixl 8 <&&>
(<&&>) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
p <&&> p' = (&&) <$> p <*> p'

infixl 8 <<&&>>
(<<&&>>) :: (a -> b -> Bool) -> (a -> b -> Bool) -> (a -> b -> Bool)
p <<&&>> p' = (<&&>) <$> p <*> p'

infixl 8 <<<&&>>>
(<<<&&>>>) :: (a -> b -> c -> Bool) -> (a -> b -> c -> Bool) -> (a -> b -> c -> Bool)
p <<<&&>>> p' = (<<&&>>) <$> p <*> p'

infixl 8 <||>
(<||>) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
p <||> p' = (||) <$> p <*> p'

infixl 8 <<||>>
(<<||>>) :: (a -> b -> Bool) -> (a -> b -> Bool) -> (a -> b -> Bool)
p <<||>> p' = (<||>) <$> p <*> p'

infixl 8 <<<||>>>
(<<<||>>>) :: (a -> b -> c -> Bool) -> (a -> b -> c -> Bool) -> (a -> b -> c -> Bool)
p <<<||>>> p' = (<<||>>) <$> p <*> p'

infixl 8 <++>
(<++>) :: (a -> [b]) -> (a -> [b]) -> (a -> [b])
p <++> p' = (++) <$> p <*> p'

infixl 8 <<++>>
(<<++>>) :: (a -> b -> [c]) -> (a -> b -> [c]) -> (a -> b -> [c])
p <<++>> p' = (<++>) <$> p <*> p'

ternary :: (a -> a -> a) -> (a -> a -> a -> a)
ternary binary a b c = a `binary` b `binary` c

quaternary :: (a -> a -> a) -> (a -> a -> a -> a -> a)
quaternary binary a b c d = a `binary` b `binary` c `binary` d

quinary :: (a -> a -> a) -> (a -> a -> a -> a -> a -> a)
quinary binary a b c d e = a `binary` b `binary` c `binary` d `binary` e

comparing :: (Ord a) => (b -> a) -> b -> b -> Ordering
comparing p x y = compare (p x) (p y)

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f = map snd . sortBy (comparing fst) . map (\x -> let y = f x in y `seq` (y,x))

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy (\x y -> f x == f y)

