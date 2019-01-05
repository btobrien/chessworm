
module Utils.Control where

(!!?) :: [a] -> Int -> Maybe a
xs !!? n = if (n < 0 || n > length xs) then Nothing else Just (xs !! n)

fromJustElse :: a -> Maybe a -> a
fromJustElse a ma = case ma of
    Nothing -> a
    Just x -> x

