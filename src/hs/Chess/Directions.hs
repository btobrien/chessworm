

module Chess.Directions where

import Chess.Squares
import Utils
import Control.Applicative
import Data.List

plus :: Square -> Square -> Bool
plus square = (file==) . fileOf <||> (rank==) . rankOf
    where
    file = fileOf square
    rank = rankOf square

toPoint :: Square -> (Int,Int)
toPoint = pair <$> fromEnum . fileOf <*> fromEnum . rankOf

cross :: Square -> Square -> Bool 
cross square = (0==) . manhattanDiff source . toPoint
    where
    source = toPoint square
    manhattanDiff (x,y) (x',y') = abs (x-x') - abs (y-y')

ring :: Square -> Square -> Bool 
ring square = (3==) . manhattanDist source . toPoint
    where
    source = toPoint square
    manhattanDist (x,y) (x',y') = abs (x-x') + abs (y-y')

bubble :: Square -> Square -> Bool 
bubble square = (<=2) . squareDist source . toPoint
    where
    source = toPoint square
    squareDist (x,y) (x',y') = squared (x-x') + squared (y-y') 

star :: Square -> Square -> Bool 
star square = plus square <||> cross square

cutoff :: [Square] -> Square -> Square -> Bool
cutoff blockers source target = or . map between $ blockers
    where
    between = betweenFiles <&&> betweenRanks <&&> (/=source) <&&> (/=target)
    betweenFiles square = (x square) `intbetween` (x source) $ (x target)
    betweenRanks square = (y square) `intbetween` (y source) $ (y target)
    intbetween c a b = 0 >= (c - a) * (c - b) 
    x = fromEnum . fileOf
    y = fromEnum . rankOf

accesible :: [Square] -> (Square -> Square -> Bool) -> Square -> Square -> Bool
accesible occupieds mobility square = mobility square <&&> (not . cutoff blockers square)
    where blockers = filter (mobility square) occupieds

