
module Chess.Move where

import Chess.Squares (Square(..))
import Chess.Pieces (Piece) 
import Chess.Soldier (Soldier)
import qualified Chess.Pieces as Piece

import Prelude hiding (any)

data Move = Move { 
    soldier :: Soldier,
    target :: Square,
    promotion :: Piece }

data Criteria = Criteria { 
    soldierMatch :: (Soldier -> Bool),
    targetMatch :: (Square -> Bool),
    promotionMatch :: (Piece -> Bool) }

parse :: String -> [Criteria]
parse = undefined

matches :: Criteria -> Move -> Bool
matches set  = do
    a <- (soldierMatch set) . soldier
    b <- (targetMatch set) . target
    c <- (promotionMatch set) . promotion
    return (a && b && c )

any :: Criteria
any = Criteria (const True) (const True) (const True)

none :: Criteria
none = Criteria (const False) (const False) (const False)
