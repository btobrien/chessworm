
module Chess.Init where

import Chess.Squares (Square(..))
import qualified Chess.Squares as Square
import Chess.Pieces (Piece)
import qualified Chess.Pieces as Piece
import Chess.Colors
import Chess.Battle
import Chess.Board
import qualified Chess.Flags as Flags

new :: Board
new = set Flags.newPair ([
    Soldier Piece.King E1 ],[
    Soldier Piece.King E8 ])

