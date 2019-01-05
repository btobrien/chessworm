
module Utils.Maybe where

fromJustElse :: a -> Maybe a -> a
fromJustElse a ma = case ma of
    Nothing -> a
    Just x -> x

