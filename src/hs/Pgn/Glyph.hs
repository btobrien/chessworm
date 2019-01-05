
module Pgn.Glyph (glyphs, toInt, toString, strip) where

import Data.Maybe
import Data.List
import Control.Applicative

import Utils.Control

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

toInt :: String -> Int
toInt g = fromJustElse 0 (elemIndex g glyphs)

toString :: Int -> String
toString n = fromJustElse "" (glyphs !!? n)

strip :: String -> (String,Int)
strip move = fromJustElse (move,0) (strip2 move <|> strip1 move)

strip2 :: String -> Maybe (String,Int)
strip2 move = if g == 0 then Nothing else Just (move',g)
    where
    g = toInt . reverse . take 2 . reverse $ move
    move' = init . init $ move

strip1 :: String -> Maybe (String,Int)
strip1 move = if g == 0 then Nothing else Just (move',g)
    where
    g = toInt [last move]
    move' = init move
