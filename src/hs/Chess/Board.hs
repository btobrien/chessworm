
module Chess.Board where

import Chess.Squares
import Chess.Pieces (Piece, pieces)
import qualified Chess.Pieces as Piece
import Chess.Sides (Side(..))
import Chess.Move (Move(Move))
import qualified Chess.Move as Move
import Chess.Flags (Flags)
import Chess.Battle
import Utils (fromJustElse)

import Prelude hiding (flip)
import Data.Maybe

type Star = (Square,[Square]); moonsOf = snd
type Constellation = (Piece,[Star]); starsOf = snd
nameOf = fst;

type Field = Battle Flags
newtype Board = Board Field deriving Show
fieldOf (Board f) = f

move :: String -> Board -> [Board]
move inp brd = do
    m <- Move.parse inp
    field <- filter (not.check) $ tryMove m (fieldOf brd)
    return $ Board (flip field)

tryMove :: Move -> Field -> [Field]
tryMove (Move name range target promotion) field = do
    constellation <- filter nameMatch $ constellations field
    star <- filter rangeMatch $ starsOf constellation
    filter (==target) $ moonsOf star
    let source = nameOf star
    let soldier = Soldier (goodSide field) (max name promotion) target
    return . (place soldier) . (pickup source) $ field
        where 
        nameMatch = (name==) . nameOf
        rangeMatch = (range `contains`) . nameOf

constellations :: Field -> [Constellation]
constellations field = do
    name <- pieces
    return (name, (stars name field))

stars :: Piece -> Field -> [Star]
stars name field = do
    soldier <- filter pieceMatch $ (soldiers.good) field
    return (location soldier, landings soldier field)
        where pieceMatch = (name==) . piece
    
landings :: Soldier -> Field -> [Square]
landings (Soldier White Piece.Pawn source) field = []
landings (Soldier Black Piece.Pawn source) field = []
landings (Soldier _ Piece.Null _) field = []
landings (Soldier s name source) field = 
    filter (not.(friendZone s field)) $ flights name source

flights :: Piece -> Square -> [Square]
flights Piece.King source = bubble source
flights Piece.Queen source = plus source ++ cross source
flights Piece.Rook source = plus source
flights Piece.Bishop source = cross source
flights Piece.Knight source = ring source

friendZone :: Side -> Field -> Square -> Bool
friendZone s f = fromJustElse False . (fmap isSame) . (who f)
    where isSame = (s==) . side

check :: Field -> Bool
check = const False

mate :: Field -> Bool
mate = undefined

set :: (Flags,Flags) -> Side -> [Soldier] -> Board
set f s = Board . (draft f s)

