
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

data Colored a = Colored { colorOf :: Color, val :: a }

instance (Show a) => Show (Colored a) where
    show cx = shade (colorOf cx) $ show (val cx)

instance Functor Colored where
    fmap f cx = Colored (colorOf cx) (f (val cx))
