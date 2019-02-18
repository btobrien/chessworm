
module Chess.Move where

import Chess.Squares
import Chess.Pieces
import Chess.Soldier (Soldier)
import Monad.Parser
import Data.Maybe
import Utils

import Prelude hiding (any)
import Control.Applicative

data Move = CastleShort | CastleLong | Move Soldier Square Piece

data Set = Set { 
    soldierMatch :: Soldier -> Bool,
    targetMatch :: Square -> Bool,
    promotionMatch :: Piece -> Bool,
    castleShort :: Bool,
    castleLong :: Bool }

tryReadMove :: String -> Maybe Set
tryReadMove "o-o" = Just shortCastle
tryReadMove "o-o-o" = Just longCastle
tryReadMove inp = tryParse getSet $ strip inp

-- TODO: have piece not include pawn
getSet :: Parser Set
getSet = do
    p <- try piece next <||> Pawn
    p' <- (fmap piece back << charback '=') <||> Nothing
    targetRank <- try rank back
    targetFile <- try file back
    sourceFile <- fmap file next <||> Nothing 
    sourceRank <- fmap rank next <||> Nothing
    let target = square (targetFile,targetRank)
    return $ matcher p (sourceFile,sourceRank) target p'

matcher :: Piece -> (Maybe File, Maybe Rank) -> Square -> Maybe Piece -> Set
matcher p source target mp = Set (squareMatcher source) (==target) promatch False False
    where promatch = case mp of
        Nothing -> (==p)
        Just p' -> (==p') 

strip :: String -> String
strip = filter ((&&) <$> (/='+') <*> (/='x')) 

any :: Set
any = Set (const True) (const True) (const True) True True

empty :: Set
empty = Set (const False) (const False) (const False) False False

shortCastle :: Set
shortCastle = Set (const False) (const False) (const False) True False

longCastle :: Set
longCastle = Set (const False) (const False) (const False) False True

contains :: Set -> Move -> Bool
contains = undefined

toSet :: Move -> Set
toSet = undefined
