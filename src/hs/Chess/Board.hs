
module Chess.Board where

import Chess.Squares
import Chess.Pieces (Piece)
import qualified Chess.Pieces as Piece
import Chess.Soldier
import Chess.Move (Move(Move))
import qualified Chess.Move as Move
import Chess.Flags (Flags)
import Chess.Battle

import Prelude hiding (flip)
import Data.Maybe
import Control.Applicative

type Field = Battle Flags
type Board = Field -- move to interface file

set :: (Flags,Flags) -> ([Soldier],[Soldier]) -> Board
set flags = (draft flags)

moves :: Move.Criteria -> Field -> [Field]
moves move field = do
    field <- filter (not.check) $ fields move field
    return (flip field)

fields :: Move.Criteria -> Field -> [Field]
fields (Move.Criteria soldierMatch targetMatch promotionMatch) field = do
    soldier <- filter soldierMatch $ (soldiers.good) field 
    target <- filter targetMatch $ radiations field soldier
    promotion <- filter promotionMatch $ candidates
    let cleaner = update (location soldier, target)
    let newSoldier = promote promotion soldier
    return . (cleanFlag cleaner) . (newSoldier `occupy` target) $ field

radiations :: Field -> Soldier -> [Square]
radiations = undefined

candidates :: [Piece]
candidates = Piece.Null : Piece.pieces

update :: (Square,Square) -> Flags -> Flags
update (from,to) flags = undefined

threatened :: Square -> Field -> Bool
threatened = undefined

throne :: Field -> Square
throne = head . (filter isKing) . (soldiers.good) 
    where isKing soldier = (Piece.King==).authority

check :: Field -> Bool
check field = threatened (throne field) field

gameover :: Field -> Bool
gameover = null . (moves Move.any)

mate :: Field -> Bool
mate = (&&) <$> check <*> gameover

stalemate :: Field -> Bool
stalemate = (&&) <$> (not.check) <*> gameover

