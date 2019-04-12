

module Chess.Battle where

import Chess.Squares
import Chess.Soldier
import Chess.Pieces -- have Soldier import for you
import Data.Maybe
import Utils

data Army a = Army {
    soldiers :: [Soldier],
    flag :: a } deriving Show

data Battle a = Battle {
    good :: Army a,
    evil :: Army a } deriving Show

locations :: Army a -> [Square]
locations = map location . soldiers

authorities :: Army a -> [Piece]
authorities = map authority . soldiers

clear :: Square -> Battle a -> Battle a
clear square (Battle g e) = 
    Battle (discharge square g) (discharge square e)

discharge :: Square -> Army a -> Army a
discharge square (Army ss f) = Army (removeFrom ss) f
    where removeFrom = filter (not . on square)

place :: Soldier -> Battle a -> Battle a
place s (Battle (Army ss f) e) =
    Battle (Army (s:ss) f) e 

flip :: Battle a -> Battle a
flip f = Battle (evil f) (good f)

setFlag :: (a -> a) -> Battle a -> Battle a
setFlag setter (Battle (Army s f) e) = 
    Battle (Army s (setter f)) e

draft :: (a,a) -> ([Soldier],[Soldier]) -> Battle a
draft (f,f') (a,a') = Battle (Army a f) (Army a' f')

friend :: Battle a -> Square -> Bool
friend battle square = any (on square) $ soldiers (good battle)

enemy :: Battle a -> Square -> Bool
enemy battle square = any (on square) $ soldiers (evil battle)

occupied :: Battle a -> Square -> Bool
occupied battle = not . isJust . (battle `at`)

occupieds :: Battle a -> [Square]
occupieds = locations . good <++> locations . evil

friendzones :: Battle a -> [Square]
friendzones = locations . good

at :: Battle a -> Square -> Maybe Soldier
at (Battle g e) square = listToMaybe $
    filter (on square) (soldiers g) ++
    filter (on square) (soldiers e) 

on :: Square -> Soldier -> Bool
on square = (square==) . location

