
module Chess.Soldier where

import Chess.Pieces (Piece)
import Chess.Squares (Square)

data Soldier = Soldier {
    authority :: Piece,
    location :: Square } deriving (Eq)

instance Show Soldier where
    show (Soldier p s) = "(" ++ show p ++ "," ++ show s ++ ")" 

promote :: Piece -> Soldier -> Soldier
promote to (Soldier a l) = Soldier to l

march :: Square -> Soldier -> Soldier
march to (Soldier a l) = Soldier a to
