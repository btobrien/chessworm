
module Chess.Fen where

import Chess.Color
import Chess.Soldier
import Utils
import Chess.Squares
import Chess.Pieces
import Data.List hiding (sortOn)

toFen :: [Colored Soldier] -> String
toFen = 
    intercalate "/" . 
    map fenOfRank . 
    reverse .
    groupRanks . 
    sortOn (location . val)

groupRanks :: [Colored Soldier] -> [[Colored Soldier]]
groupRanks soldiers = map rankFilter ranks
    where 
    rankFilter rank = filter ((rank==) . rankOf . location . val) soldiers

fenOfRank :: [Colored Soldier] -> String
fenOfRank [] = show dimension
fenOfRank soldiers = pre ++ (concatMap fenOfPair $ buddies soldiers) ++ post
    where
    pre = showSpaces . fromEnum . fileOf . location . val . head $ soldiers
    post = showPiece . last $ soldiers

fenOfPair :: (Colored Soldier, Colored Soldier) -> String
fenOfPair (x,y) = showPiece x ++ showSpaces (spacesBetween x y)
    where spacesBetween x y = subtract 1 $ diffOn (location . val) x y

showSpaces :: Int -> String
showSpaces diff = if diff /= 0 then show diff else "" 

showPiece :: Colored Soldier -> String
showPiece soldier = show . fmap authority $ soldier
