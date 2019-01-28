
module Chess.Soldier where

import Chess.Pieces (Piece)
import Chess.Squares (Square)

data Soldier = Soldier {
    authority :: Piece,
    location :: Square }

instance Show Soldier where
    show (Soldier p s) = "(" ++ show p ++ "," ++ show s ++ ")" 

promote :: Piece -> Soldier -> Soldier
promote = undefined

