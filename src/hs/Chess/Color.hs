
module Chess.Color where

import Data.Char

data Color = White | Black 
    deriving (Show,Eq)

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

data Colored a = Colored { color :: Color, val :: a } deriving Show

