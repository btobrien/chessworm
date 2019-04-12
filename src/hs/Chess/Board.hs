
module Chess.Board where

import Chess.Squares
import Chess.Pieces
import Chess.Soldier
import Chess.Move (Set(..))
import qualified Chess.Move as Move
import Chess.Flags
import Chess.Battle
import Utils
import Chess.Directions
import Chess.Color

import Prelude hiding (flip)
import Data.Maybe
import Data.List
import Control.Applicative

type Field = Battle Flags
type Board = Field -- move to interface file as newtype
set = draft

moves :: Move.Set -> Field -> [Field]
moves move field = do
    field <- filter (not . check) $ fields move field
    return (flip field)

fields :: Move.Set -> Field -> [Field]
fields = 
    passants <<++>> 
    castles <<++>>
    placements

placements :: Move.Set -> Field -> [Field]
placements move field = do
    soldier <- filter (soldierMatch move) $ (soldiers . good) field 
    target <- filter (targetMatch move) $ targets soldier field 
    promotion <- filter (promotionMatch move) $ promotions target (authority soldier)
    let source = location soldier
    let newSoldier = promote promotion . march target $ soldier
    let setter = update field (source,target)
    return .
        setFlag setter .
        place newSoldier .
        clear source $ field

targets :: Soldier -> Field -> [Square]
targets (Soldier piece square) field = movements piece square \\ friendzones field
    where
    movements King = bubble
    movements Queen = unblocked (occupieds field) star
    movements Rook = unblocked (occupieds field) plus
    movements Bishop = unblocked (occupieds field) cross
    movements Knight = ring
    movements Pawn = pawnMovements field 

pawnMovements :: Field -> Square -> [Square]
pawnMovements = pawnPush <<++>> pawnDoublePush <<++>> pawnCapture

pawnPush :: Field -> Square -> [Square]
pawnPush field square | True = undefined --north (generate north square) <&&> not . occupied field
pawnPush field sqaure | otherwise = undefined --south square <&&> not . occupied field

pawnDoublePush :: Field -> Square -> [Square]
pawnDoublePush = undefined 
--pawnDoublePush field | whiteToMove = north square <&&> not . occupied field
--pawnDoublePush field | otherwise = south square <&&> not . occupied field

pawnCapture = undefined

sndRank :: Field -> Square -> Bool
sndRank = undefined

promotions :: Square -> Piece -> [Piece]
promotions square Pawn | (rankOf square == R8) || (rankOf square == R1) = pieces \\ [Pawn, King]
promotions _ piece = [piece] 

castles :: Move.Set -> Field -> [Field]
castles = castlesShort <<++>> castlesLong

castlesShort :: Move.Set -> Field -> [Field]
castlesShort move field | not (castleShort move) = []
castlesShort move field | (not . short . flag . good) field = []
castlesShort move field = castle (G,H,F) field

castlesLong :: Move.Set -> Field -> [Field]
castlesLong move field | not (castleLong move) = []
castlesLong move field | (not . long . flag . good) field = []
castlesLong move field = castle (C,A,D) field

castle :: (File,File,File) -> Field -> [Field]
castle (g,h,f) field =
    if 
        any (threatened field) [king .. newKing] ||
        any (occupied field) (tail [king .. newKing])
    then []
    else
        (:[]) .
        setFlag updateShort .
        (rook `moveTo` newRook) .
        (king `moveTo` newKing) $ field
    where
    king = throneOf field
    rank = rankOf king
    newKing = toSquare (g,rank)
    rook = toSquare(h,rank)
    newRook = toSquare(f,rank)


moveTo :: Square -> Square -> Field -> Field
moveTo source target field =
    case field `at` source of
        Nothing -> field
        Just soldier -> place (march target soldier) . clear target $ field

passants :: Move.Set -> Field -> [Field]
passants move = const []

-- TODO
update :: Field -> (Square,Square) -> (Flags -> Flags)
update field (from,to) = id

updateShort :: Flags -> Flags
updateShort = undefined 

updateLong :: Flags -> Flags
updateLong = undefined 

-- TODO
passant :: Field -> (Square,Square) -> Maybe Square
passant field (from,to) = Nothing

-- TODO
threatened :: Field -> Square -> Bool
threatened field square = False 

kingOf :: Field -> Soldier
kingOf = head . filter isKing . soldiers . good
    where isKing = (King==) . authority

throneOf :: Field -> Square
throneOf = location . kingOf

check :: Field -> Bool
check field = threatened field (throneOf field)

gameover :: Board -> Bool
gameover = null . moves Move.any

checkmate :: Board -> Bool
checkmate = check <&&> gameover

stalemate :: Board -> Bool
stalemate = not . check <&&> gameover

dump :: Board -> [Colored Soldier]
dump = const []

implication :: Board -> Board -> Maybe Move.Move
implication = undefined
