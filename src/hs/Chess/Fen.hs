
module Chess.Fen where

import Chess.Color
import Chess.Soldier
import Utils

toFen :: [Colored Soldier] -> String
toFen = sortedToFen . sortOn (location . val)

sortedToFen = const "test"

fenPiece :: (Colored Soldier, Colored Soldier) -> String
fenPiece = undefined
