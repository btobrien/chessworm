
module Chess.Sides where

import Data.Char

data Name = White | Black 

shade :: Name -> String -> String
shade White = id
shade Black = map toUpper 

