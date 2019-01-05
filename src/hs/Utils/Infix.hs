
module Utils.Infix where

(!!?) :: [a] -> Int -> Maybe a
xs !!? n = if (n < 0 || n > length xs) then Nothing else Just (xs !! n)

