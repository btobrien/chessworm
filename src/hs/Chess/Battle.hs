

module Chess.Battle where

import Chess.Squares
import Chess.Soldier
import Utils (fromJustElse)

import Data.Maybe

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

cleanFlag :: (a -> a) -> Battle a -> Battle a
cleanFlag = undefined

draft :: (a,a) -> ([Soldier],[Soldier]) -> Battle a
draft (f,f') (g,e) = Battle (Army g f) (Army e f')

friend :: Battle a -> Square -> Bool
friend battle square = any (on square) $
    soldiers (good battle)

enemy :: Battle a -> Square -> Bool
enemy battle square = any (on square) $
    soldiers (evil battle)

vacant :: Battle a -> Square -> Bool
vacant battle = isJust . (battle `at`)

at :: Battle a -> Square -> Maybe Soldier
at (Battle g e) square = listToMaybe $
    filter (on square) (soldiers g) ++
    filter (on square) (soldiers e) 

on :: Square -> Soldier -> Bool
on square = (square==) . location

