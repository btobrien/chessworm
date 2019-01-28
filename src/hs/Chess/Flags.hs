
module Chess.Flags where

import Chess.Move
import Chess.Squares (Square(Null))

import Control.Applicative

data Flags = F {
    short :: Bool,
    long  :: Bool,
    passant :: Square }

new :: Flags
new = F True True Null

newPair :: (Flags,Flags)
newPair = (new,new)

instance Show Flags where
    show (F s l e) = (showIfSet "K" s) ++ (showIfSet "Q" l) ++ (show e)
            where showIfSet s b = if b then s else ""

isShort :: String ->  Bool
isShort = undefined

isLong :: String -> Bool
isLong = undefined

isCastle = (||) <$> isShort <*> isLong
