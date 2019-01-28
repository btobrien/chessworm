

module Chess.Battle where

import Chess.Squares (Square)
import Chess.Pieces (Piece)
import Chess.Sides (Side,shade)

import Data.Maybe

data Soldier = Soldier {
    side :: Side,
    piece :: Piece,
    location :: Square }

instance Show Soldier where
    show (Soldier s p l) = "(" ++ shade s (show p) ++ "," ++ show l ++ ")" 

data Army a = Army {
    soldiers :: [Soldier],
    flag :: a } deriving Show

data Battle a = Battle {
    good :: Army a,
    evil :: Army a } deriving Show

pickup :: Square -> Battle a -> Battle a
pickup loc f = undefined

place :: Soldier -> Battle a -> Battle a
place s f = undefined

flip :: Battle a -> Battle a
flip f = Battle (evil f) (good f)

goodSide :: Battle a -> Side
goodSide = side . head . soldiers . good

setFlag :: a -> Battle a -> Battle a
setFlag = undefined

getFlag :: Battle a -> a
getFlag = undefined

draft :: (a,a) -> Side -> [Soldier] -> Battle a
draft (a,a') goodSide ss = Battle 
    (Army (filter isGood ss) a)
    (Army (filter (not.isGood) ss) a')
        where isGood = (goodSide==) . side

who :: Battle a -> Square -> Maybe Soldier
who (Battle g e) square = listToMaybe $
    filter (on square) (soldiers g) ++
    filter (on square) (soldiers e) 

on :: Square -> Soldier -> Bool
on square = (square==) . location

