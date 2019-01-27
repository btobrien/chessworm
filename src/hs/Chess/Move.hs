
module Chess.Move where

import qualified Chess.Squares as Square
import qualified Chess.Pieces as Piece

data Move = CastleShort | CastleLong | Move Piece.Name Square.Range Square.Location Piece.Name

parse :: String -> Maybe Move
parse = undefined

castleShort :: Move -> Bool
castleShort CastleShort = True
castleShort _ = False

castleLong :: Move -> Bool
castleLong CastleLong = True
castleLong _ = False

castle = (||) <?> castleShort <*> castleLong

piece :: Move -> Piece.Name
piece CastleShort = Piece.King
piece CastleLong = Piece.King
piece (Move p _ _ ) = p

range :: Move -> Square.Range
range CastleShort = Square.All
range CastleLong = Square.All
range (Move _ r _) = r 

target :: Move -> Square.Location
target CastleShort = Square.Null
target CastleLong = Square.Null
target (Move _ _ l) = l 

promotion :: String -> Maybe Piece.Name
promotion = undefined
