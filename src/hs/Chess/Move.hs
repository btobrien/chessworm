
module Chess.Move where

import Chess.Squares
import Chess.Pieces
import Chess.Soldier
import Monad.Parser
import Data.Maybe
import Utils

import Prelude hiding (any)
import Control.Applicative

data Move = 
    CastleShort | CastleLong |
    Move Piece (Maybe File, Maybe Rank) Square (Maybe Piece)
        deriving Show

data Set = Set { 
    soldierMatch :: Soldier -> Bool,
    targetMatch :: Square -> Bool,
    promotionMatch :: Piece -> Bool,
    castleShort :: Bool,
    castleLong :: Bool }

tryRead :: String -> Maybe Move
tryRead = tryRead' . strip . trim
    where 
    strip = filter $ (&&) <$> (/='+') <*> (/='x')
    tryRead' "o-o" = Just CastleShort
    tryRead' "o-o-o" = Just CastleLong
    tryRead' inp = tryParse moveParser inp

moveParser :: Parser Move
moveParser = do
    p <- get piece next <||> Pawn
    p' <- try piece back << charback '=' <||> Nothing
    targetRank <- get rank back
    targetFile <- get file back
    sourceFile <- try file next
    sourceRank <- try rank next
    let target = square (targetFile,targetRank)
    return $ Move p (sourceFile,sourceRank) target p'

toSet :: Move -> Set
toSet (Move p (mf,mr) target mp) = 
    Set soldierSet (==target) promotionSet False False
        where 

        promotionSet = case mp of
            Nothing -> (==p)
            Just p' -> (==p') 

        soldierSet = ternary (&&) 
            <$> (==p) . authority
            <*> matchMaybe mf . fileOf . location
            <*> matchMaybe mr . rankOf . location

        matchMaybe mx = case mx of
            Nothing -> const True
            Just x -> (==x)

any :: Set
any = Set (const True) (const True) (const True) True True

empty :: Set
empty = Set (const False) (const False) (const False) False False

shortCastle :: Set
shortCastle = Set (const False) (const False) (const False) True False

longCastle :: Set
longCastle = Set (const False) (const False) (const False) False True
