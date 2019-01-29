
module Chess.Init where

import Chess.Squares (Square(..))
import Chess.Pieces
import Chess.Soldier
import Chess.Colors
import qualified Chess.Flags as Flags
import Chess.Board

new :: Board
new = set Flags.newPair ([
    Soldier King E1 ],[
    Soldier King E8 ])

