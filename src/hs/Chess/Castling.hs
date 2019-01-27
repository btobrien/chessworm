
module Chess.Castling where

import Chess.Move

data Flags = F {
    Short :: Bool,
    Long  :: Bool }

new :: Flags
new = F True True

check :: Move -> Flags -> Maybe Flags
check = undefined

instance Show Flags where
    show (F s l) = 
        (showIfSet "K" s) ++
        (showIfSet "Q" l) ++
            where showIfSet s b = if b then s else ""
