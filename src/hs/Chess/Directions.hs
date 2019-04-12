

module Chess.Directions where

import Chess.Squares
import Utils
import Control.Applicative
import Data.List

isPlus :: Square -> Square -> Bool
isPlus square = (file==) . fileOf <||> (rank==) . rankOf
    where
    file = fileOf square
    rank = rankOf square

toPoint :: Square -> (Int,Int)
toPoint = pair <$> fromEnum . fileOf <*> fromEnum . rankOf

isCross :: Square -> Square -> Bool 
isCross square = (0==) . manhattanDiff source . toPoint
    where
    source = toPoint square
    manhattanDiff (x,y) (x',y') = abs (x-x') - abs (y-y')

isRing :: Square -> Square -> Bool 
isRing square = (3==) . manhattanDist source . toPoint
    where
    source = toPoint square
    manhattanDist (x,y) (x',y') = abs (x-x') + abs (y-y')

isBubble :: Square -> Square -> Bool 
isBubble square = (<=2) . squareDist source . toPoint
    where
    source = toPoint square
    squareDist (x,y) (x',y') = squared (x-x') + squared (y-y') 

isStar :: Square -> Square -> Bool 
isStar = isPlus <<||>> isCross

north = undefined

cutoff :: [Square] -> Square -> Square -> Bool
cutoff blockers source target = any between blockers
    where
    between = betweenFiles <&&> betweenRanks <&&> (/=source) <&&> (/=target)
    betweenFiles square = (x square) `intbetween` (x source) $ (x target)
    betweenRanks square = (y square) `intbetween` (y source) $ (y target)
    intbetween c a b = 0 >= (c - a) * (c - b) 
    x = fromEnum . fileOf
    y = fromEnum . rankOf

unblocked :: [Square] -> (Square -> [Square]) -> (Square -> [Square])
unblocked blockers mobility square = candidates \\ generate (cutoff relevantBlockers square)
    where
    candidates = mobility square
    relevantBlockers = blockers `intersect` candidates

generate :: (Square -> Bool) -> [Square]
generate mobility = filter mobility squares

plus = generate . isPlus
cross = generate . isCross
ring = generate . isRing
bubble = generate . isBubble
star = generate . isStar

