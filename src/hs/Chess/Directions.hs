

module Chess.Directions where

import Chess.Squares
import Utils
import Control.Applicative
import Data.List

plus :: Square -> [Square]
plus square = filter plusmatch squares
    where
    file = fileOf square
    rank = rankOf square
    plusmatch = (||) <$> (file==) . fileOf <*> (rank==) . rankOf

toPoint :: Square -> (Int,Int)
toPoint = pair <$> fromEnum . fileOf <*> fromEnum . rankOf

cross :: Square -> [Square]
cross square = filter ((0==) . manhattanDiff source . toPoint) squares
    where
    source = toPoint square
    manhattanDiff (x,y) (x',y') = abs (x-x') - abs (y-y')

ring :: Square -> [Square]
ring square = filter ((3==) . manhattanDist source . toPoint) squares
    where
    source = toPoint square
    manhattanDist (x,y) (x',y') = abs (x-x') + abs (y-y')

bubble :: Square -> [Square]
bubble square = filter ((<=2) . squareDist source . toPoint) squares
    where
    source = toPoint square
    squareDist (x,y) (x',y') = squared (x-x') + squared (y-y') 

star :: Square -> [Square]
star = nub . ((++) <$> plus <*> cross)

cutoff :: Square -> [Square] -> Square -> Bool
cutoff source border = const False

