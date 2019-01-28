
module Chess.Flags where

import Chess.Move
import Chess.Colors
import Chess.Squares (Square(Null))

import Control.Applicative

data Flags = F {
    color :: Color,
    short :: Bool,
    long  :: Bool,
    passant :: Square }

new :: Color -> Flags
new c = F c True True Null

newPair :: (Flags,Flags)
newPair = (new first,new second)

instance Show Flags where
    show (F c s l e) = 
        shade c $
        (showIfSet "K" s) ++ (showIfSet "Q" l) ++ (show e)
            where showIfSet s b = if b then s else ""

isShort :: String ->  Bool
isShort = undefined

isLong :: String -> Bool
isLong = undefined

isCastle = (||) <$> isShort <*> isLong
