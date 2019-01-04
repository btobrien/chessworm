module Glyph where
import Control.Applicative
import Data.Maybe

import Data.List

glyphs = [
    "",
    "!",
    "?",
    "!!",
    "??",
    "!?",
    "?!",
    "□", 
    "<singular move>",
    "<worst move>",
    "=",
    "<equal chances, quiet position>",
    "<equal chances, active position>",
    "∞",
    "+=",
    "+=",
    "+-",
    "-+",
    "+−",
    "−+"]

(!!?) :: [a] -> Int -> Maybe a
xs !!? n = if (n < 0 || n > length xs) then Nothing else Just (xs !! n)

glyphToInt :: String -> Int
glyphToInt g = case elemIndex g glyphs of
                    Nothing -> 0
                    Just x -> x

showGlyph :: Int -> String
showGlyph n = case glyphs !!? n of
    Nothing -> ""
    Just g -> g

fromJustElse :: a -> Maybe a -> a
fromJustElse a ma = case ma of
    Nothing -> a
    Just x -> x

strip :: String -> (String,Int)
strip move = fromJustElse (move,0) (strip2 move <|> strip1 move)

strip2 :: String -> Maybe (String,Int)
strip2 move = if g == 0 then Nothing else Just (move',g)
    where
    g = glyphToInt . reverse . take 2 . reverse $ move
    move' = init . init $ move

strip1 :: String -> Maybe (String,Int)
strip1 move = if g == 0 then Nothing else Just (move',g)
    where
    g = glyphToInt [last move]
    move' = init move
