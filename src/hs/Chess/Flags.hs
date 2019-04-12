
module Chess.Flags where

import Chess.Move
import Chess.Color
import Chess.Squares (Square)
import Utils (showJust)

import Control.Applicative

data Flags = Flags {
    color :: Color,
    short :: Bool,
    long  :: Bool,
    passant :: Maybe Square }

new :: Color -> Flags
new c = Flags c True True Nothing

newPair :: (Flags,Flags)
newPair = (new first,new second)

instance Show Flags where
    show (Flags c s l e) = 
        shade c $
        showIf s "K" ++ showIf l "Q" ++ showJust e
            where showIf b s = if b then s else ""
