

module Chess.Directions where

import Chess.Squares
import Utils
import Control.Applicative
import Data.List

rankDiff :: Square -> Square -> Int
rankDiff a b = intRank a - intRank b

isPlus :: Square -> Square -> Bool
isPlus start = (file==) . fileOf <||> (rank==) . rankOf
    where
    file = fileOf start
    rank = rankOf start

isCross :: Square -> Square -> Bool 
isCross start = (0==) . manhattanDiff source . toPoint
    where
    source = toPoint start
    manhattanDiff (x,y) (x',y') = abs (x-x') - abs (y-y')

isRing :: Square -> Square -> Bool 
isRing start = (3==) . manhattanDist source . toPoint
    where
    source = toPoint start
    manhattanDist (x,y) (x',y') = abs (x-x') + abs (y-y')

isBubble :: Square -> Square -> Bool 
isBubble start = (<=2) . squareDist source . toPoint
    where
    source = toPoint start
    squareDist (x,y) (x',y') = squared (x-x') + squared (y-y') 

isStar :: Square -> Square -> Bool 
isStar = isPlus <<||>> isCross

isDirectAbove :: Int -> Square -> Square -> Bool
isDirectAbove n start target = (fileOf start == fileOf target) && (rankDiff target start == n)

isDirectBelow :: Int -> Square -> Square -> Bool
isDirectBelow n = flip (isDirectAbove n)

isDiagAbove :: Square -> Square -> Bool
isDiagAbove start target = (isCross start target) && (rankDiff target start == 1)

isDiagBelow :: Square -> Square -> Bool
isDiagBelow = flip isDiagAbove

cutoff :: [Square] -> (Square -> Square -> Bool)
cutoff blockers start target = any between blockers
    where
    between = betweenFiles <&&> betweenRanks <&&> (/=start) <&&> (/=target)
    betweenFiles square = (x square) `intbetween` (x start) $ (x target)
    betweenRanks square = (y square) `intbetween` (y start) $ (y target)
    intbetween c a b = 0 >= (c - a) * (c - b) 
    x = intFile
    y = intFile

unblocked :: [Square] -> (Square -> Square -> Bool) -> (Square -> Square -> Bool)
unblocked blockers mobility start = mobility start <&&> not . cutoff relevantBlockers start
    where
    relevantBlockers = filter (mobility start) blockers

generate :: (Square -> Bool) -> [Square]
generate f = filter f squares

plus = generate . isPlus
cross = generate . isCross
ring = generate . isRing
bubble = generate . isBubble
star = generate . isStar

upFrom = undefined
downFrom = undefined
