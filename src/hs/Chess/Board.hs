
module Chess.Board where

import Chess.Squares
import Chess.Pieces (Piece, pieces)
import qualified Chess.Pieces as Piece
import Chess.Move (Move(Move))
import qualified Chess.Move as Move
import Chess.Flags (Flags)
import Chess.Battle

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
    let soldier = Soldier (max name promotion) source
    return $ (soldier `occupy` target) field
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
    return (location soldier, moons soldier field)
        where pieceMatch = (name==) . piece
    
moons :: Soldier -> Field -> [Square]
moons (Soldier Piece.Pawn source) field = []
moons (Soldier Piece.Null _) _ = []
moons (Soldier name source) field = 
    filter (not.(friend field)) $ landings name source

landings :: Piece -> Square -> [Square]
landings Piece.King source = bubble source
landings Piece.Queen source = plus source ++ cross source
landings Piece.Rook source = plus source
landings Piece.Bishop source = cross source
landings Piece.Knight source = ring source

check :: Field -> Bool
check = const False

mate :: Field -> Bool
mate = undefined

set :: (Flags,Flags) -> ([Soldier],[Soldier]) -> Board
set f = Board . (draft f)

