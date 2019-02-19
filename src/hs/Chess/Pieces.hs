
module Chess.Pieces where

data Piece = Pawn | Knight | Bishop | Rook | Queen | King
    deriving (Enum, Eq, Ord)

pieces :: [Piece]
pieces = enumFrom . toEnum $ 0

piece :: Char -> Maybe Piece
piece 'K' = Just King
piece 'Q' = Just Queen
piece 'R' = Just Rook
piece 'B' = Just Bishop
piece 'N' = Just Knight
piece 'P' = Just Pawn
piece _ = Nothing

instance Show Piece where
    show King = "K"
    show Queen = "Q"
    show Rook = "R"
    show Bishop = "B"
    show Knight = "N"
    show Pawn = "P"

title :: Piece -> Char
title = head . show

value :: Piece -> Int
value Pawn = 1
value Knight = 3
value Bishop = 3
value Rook = 5
value Queen = 9
value King = maxBound


