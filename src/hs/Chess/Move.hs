
module Chess.Move where

import Chess.Squares (Square(..),Range,sqRange)
import Chess.Pieces (Piece) 
import qualified Chess.Pieces as Piece

data Move = Move { 
    piece :: Piece,
    range :: Range,
    target :: Square,
    promotion :: Piece }

parse :: String -> [Move]
parse = const [Move Piece.King (sqRange E1) E2 Piece.Null]
