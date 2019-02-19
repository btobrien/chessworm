
module Chess.Init where

import Chess.Squares (Square(..))
import Chess.Pieces
import Chess.Soldier
import Chess.Color
import qualified Chess.Flags as Flags
import Chess.Board
import qualified Chess.Move as Move
import Data.Maybe
import Chess.Fen

new :: Board
new = set Flags.newPair ([
    Soldier King E1 ],[
    Soldier King E8 ])

readmove :: String -> Board -> Board
readmove m = head . moves (Move.toSet . fromJust $ Move.tryRead m)
