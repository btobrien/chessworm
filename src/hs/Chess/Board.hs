
module Chess.Board where

import Chess.Squares
import Chess.Pieces
import Chess.Soldier
import Chess.Move (Set(..))
import qualified Chess.Move as Move
import Chess.Flags (Flags)
import Chess.Battle
import Utils (ternary)

import Prelude hiding (flip)
import Data.Maybe
import Control.Applicative

type Field = Battle Flags
type Board = Field -- move to interface file as newtype
type Target = Square
set = draft

moves :: Move.Set -> Field -> [Field]
moves move field = do
    field <- filter (not . check) $ fields move field
    return (flip field)

fields :: Move.Set -> Field -> [Field]
fields move = ternary (++)
    <$> passants move
    <*> castles move
    <*> placements move

placements :: Move.Set -> Field -> [Field]
placements move field = do
    soldier <- filter (soldierMatch move) $ (soldiers . good) field 
    target <- filter (targetMatch move) $ targets soldier field 
    promotion <- filter (promotionMatch move) $ promotions (authority soldier) target
    let source = location soldier
    let newSoldier = promote promotion . march target $ soldier
    let setter = update field (source,target)
    return .
        setFlag setter .
        place newSoldier .
        clear source $ field

castles :: Move.Set -> Field -> [Field]
castles move = const []

passants :: Move.Set -> Field -> [Field]
passants move = const []

targets :: Soldier -> Field -> [Target]
targets soldier field = [E1,E2]

promotions :: Piece -> Target -> [Piece]
promotions Pawn sq | (rank sq == R8) || (rank sq == R1) = pieces
promotions piece _ = [piece] 

update :: Field -> (Square,Square) -> (Flags -> Flags)
update field (from,to) = id

passant :: Field -> (Square,Square) -> Maybe Square
passant field (from,to) = Nothing

threatened :: Square -> Field -> Bool
threatened square field = False 

throne :: Field -> Square
throne = location . head . filter isKing . soldiers . good
    where isKing = (King==) . authority

check :: Field -> Bool
check field = threatened (throne field) field

gameover :: Board -> Bool
gameover = null . moves Move.any

checkmate :: Board -> Bool
checkmate = (&&) <$> check <*> gameover

stalemate :: Board -> Bool
stalemate = (&&) <$> (not . check) <*> gameover

implication :: Board -> Board -> Maybe Move.Move
implication = undefined
