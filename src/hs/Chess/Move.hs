
module Chess.Move where

import Chess.Squares (Square)
import Chess.Pieces (Piece) 
import Chess.Soldier (Soldier)
import Utils (ternary)

import Prelude hiding (any)
import Control.Applicative

data Move = Move { 
    soldier :: Soldier,
    target :: Square,
    promotion :: Piece }

data Set = Set { 
    soldierMatch :: (Soldier -> Bool),
    targetMatch :: (Square -> Bool),
    promotionMatch :: (Piece -> Bool) }

parse :: String -> [Set]
parse = undefined

matches :: Set -> Move -> Bool
matches set  = ternary (&&)
    <$> (soldierMatch set) . soldier
    <*> (targetMatch set) . target
    <*> (promotionMatch set) . promotion

any :: Set
any = Set (const True) (const True) (const True)

none :: Set
none = Set (const False) (const False) (const False)

shortCastle :: Set
shortCastle = undefined

longCastle :: Set
longCastle = undefined
