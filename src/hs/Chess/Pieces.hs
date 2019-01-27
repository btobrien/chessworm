
module Chess.Pieces where

data Name = King | Queen | Rook | Bishop | Knight | Pawn | Null

instance Show Name where
    show King = "K"
    show Queen = "Q"
    show Rook = "R"
    show Bishop = "B"
    show Knight = "N"
    show Pawn = "P"
    show Null = " "
