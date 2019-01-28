
module Chess.Flags where

import Chess.Move

import Control.Applicative

data Flags = F {
    short :: Bool,
    long  :: Bool }

new :: Flags
new = F True True

check :: Move -> Flags -> Maybe Flags
check = undefined

instance Show Flags where
    show (F s l) = (showIfSet "K" s) ++ (showIfSet "Q" l)
            where showIfSet s b = if b then s else ""


isShort :: String ->  Bool
isShort = undefined

isLong :: String -> Bool
isLong = undefined

isCastle = (||) <$> isShort <*> isLong
