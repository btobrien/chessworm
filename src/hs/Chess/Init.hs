
module Chess.Init where

import Chess.Squares (Square(..))
import qualified Chess.Pieces as Piece
import  Chess.Soldier
import Chess.Colors
import qualified Chess.Flags as Flags
import Chess.Board

new :: Board
new = set Flags.newPair ([
    Soldier Piece.King E1 ],[
    Soldier Piece.King E8 ])

