
module Chess.Colors where

import Data.Char

data Color = White | Black 
    deriving Eq

first :: Color
first = White

second :: Color
second = next first

next :: Color -> Color
next White = Black
next Black = White

shade :: Color -> String -> String
shade White = id
shade Black = map toLower 

