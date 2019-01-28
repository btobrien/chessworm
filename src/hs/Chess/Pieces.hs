
module Chess.Pieces where

data Piece = Null | Pawn | Knight | Bishop | Rook | Queen | King
    deriving (Enum, Eq, Ord)

pieces :: [Piece]
pieces = enumFrom . toEnum $ 1

instance Show Piece where
    show King = "K"
    show Queen = "Q"
    show Rook = "R"
    show Bishop = "B"
    show Knight = "N"
    show Pawn = "P"
    show Null = " "

value :: Piece -> Int
value Null = 0
value Pawn = 1
value Knight = 3
value Bishop = 3
value Rook = 5
value Queen = 9
value King = maxBound


