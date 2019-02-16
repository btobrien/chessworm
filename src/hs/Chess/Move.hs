
module Chess.Move where

import Chess.Squares
import Chess.Pieces
import Chess.Soldier (Soldier)
import Monad.Parser
import Data.Maybe
import Utils


import Prelude hiding (any)
import Control.Applicative

data Move = Move { 
    soldier :: Soldier,
    target :: Square,
    promotion :: Piece }

data Set = Set { 
    soldierMatch :: Soldier -> Bool,
    targetMatch :: Square -> Bool,
    promotionMatch :: Piece -> Bool,
    castleShort :: Bool,
    castleLong :: Bool }

parse :: String -> [Set]
parse "o-o" = [shortCastle]
parse "o-o-o" = [longCastle]

getSet :: Parser Set
getSet = undefined

getPiece :: Parser Piece
getPiece = fmap (fromMaybe Pawn . piece) letter

getTarget :: Parser Square
getTarget = undefined

strip :: String -> String
strip = filter ((&&) <$> (/='+') <*> (/='x')) 

any :: Set
any = Set (const True) (const True) (const True) True True

none :: Set
none = Set (const False) (const False) (const False) False False

shortCastle :: Set
shortCastle = Set (const False) (const False) (const False) True False

longCastle :: Set
longCastle = Set (const False) (const False) (const False) False True

matches :: Set -> Move -> Bool
matches set  = ternary (&&)
    <$> soldierMatch set . soldier
    <*> targetMatch set . target
    <*> promotionMatch set . promotion

