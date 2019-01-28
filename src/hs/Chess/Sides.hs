
module Chess.Sides where

import Data.Char

data Side = White | Black 
    deriving Eq

first :: Side
first = White

next :: Side -> Side
next White = Black
next Black = White

shade :: Side -> String -> String
shade White = id
shade Black = map toLower 

