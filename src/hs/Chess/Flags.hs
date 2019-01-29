
module Chess.Flags where

import Chess.Move
import Chess.Colors
import Chess.Squares (Square)
import Utils (showJust)

import Control.Applicative

data Flags = F {
    color :: Color,
    short :: Bool,
    long  :: Bool,
    passant :: Maybe Square }

new :: Color -> Flags
new c = F c True True Nothing

newPair :: (Flags,Flags)
newPair = (new first,new second)

instance Show Flags where
    show (F c s l e) = 
        shade c $
        (showIf s "K") ++ (showIf l "Q") ++ (showJust e)
            where showIf b s = if b then s else ""

isShort :: String ->  Bool
isShort = undefined

isLong :: String -> Bool
isLong = undefined

isCastle = (||) <$> isShort <*> isLong
