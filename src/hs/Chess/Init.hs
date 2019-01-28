
module Chess.Init where

import Chess.Squares (Square(..))
import qualified Chess.Squares as Square
import Chess.Pieces (Piece)
import qualified Chess.Pieces as Piece
import Chess.Sides
import Chess.Battle
import Chess.Board
import qualified Chess.Flags as Flags

new :: Board
new = set Flags.newPair White [
    Soldier White Piece.King E1,
    Soldier Black Piece.King E8]

