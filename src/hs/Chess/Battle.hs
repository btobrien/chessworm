

module Chess.Battle where

import Chess.Squares (Square)
import Chess.Pieces (Piece)
import Utils (fromJustElse)

import Data.Maybe

data Soldier = Soldier {
    piece :: Piece,
    location :: Square }

instance Show Soldier where
    show (Soldier p s) = "(" ++ show p ++ "," ++ show s ++ ")" 

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

occupy :: Soldier -> Square -> Battle a -> Battle a
occupy = undefined

flip :: Battle a -> Battle a
flip f = Battle (evil f) (good f)

setFlag :: a -> Battle a -> Battle a
setFlag = undefined

getFlag :: Battle a -> a
getFlag = undefined

draft :: (a,a) -> ([Soldier],[Soldier]) -> Battle a
draft (f,f') (g,e) = Battle (Army g f) (Army e f')

friend :: Battle a -> Square -> Bool
friend battle square = any (on square) $
    soldiers (good battle)

enemy :: Battle a -> Square -> Bool
enemy battle square = any (on square) $
    soldiers (evil battle)

vacant :: Battle a -> Square -> Bool
vacant battle = isJust . (who battle)

who :: Battle a -> Square -> Maybe Soldier
who (Battle g e) square = listToMaybe $
    filter (on square) (soldiers g) ++
    filter (on square) (soldiers e) 

on :: Square -> Soldier -> Bool
on square = (square==) . location

