
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
    placements <<++>> 
    castles <<++>>
    passants

placements :: Move.Set -> Field -> [Field]
placements move field = do
    soldier <- filter (soldierMatch move) $ (soldiers . good) field 
    target <- filter (targetMatch move) $ targets soldier field 
    promotion <- filter (promotionMatch move) $ promotions target (authority soldier)
    let start = location soldier
    let newSoldier = promote promotion . march target $ soldier
    let setter = update field (start,target)
    return .
        setFlag setter .
        place newSoldier .
        clear start $ field

targets :: Soldier -> Field -> [Square]
targets (Soldier piece start) field = generate $ isTarget piece start <&&> not . isFriend field
    where
    isTarget King = isBubble
    isTarget Queen = unblocked (occupieds field) isStar
    isTarget Rook = unblocked (occupieds field) isPlus
    isTarget Bishop = unblocked (occupieds field) isCross
    isTarget Knight = isRing
    isTarget Pawn = isPawnTarget field 

isPawnTarget :: Field -> Square -> Square -> Bool
isPawnTarget = isPawnPush <<<||>>> isPawnPush2 <<<||>>> isPawnCapture

isPawnPush :: Field -> Square -> Square -> Bool
isPawnPush field start | isWhite field = isDirectAbove 1 start <&&> isVacant field
isPawnPush field start | otherwise = isDirectBelow 1 start <&&> isVacant field

isPawnPush2 :: Field -> Square -> Square -> Bool
isPawnPush2 field start | isWhite field && (rankOf start == R2) =
    unblocked (occupieds field) (isDirectAbove 2) start <&&> isVacant field
isPawnPush2 field start | not (isWhite field) && (rankOf start == R7) =
    unblocked (occupieds field) (isDirectBelow 2) start <&&> isVacant field
isPawnPush2 field start | otherwise = const False

isPawnCapture :: Field -> Square -> Square -> Bool
isPawnCapture field start | isWhite field =  isDiagAbove start <&&> isEnemy field
isPawnCapture field start | otherwise = isDiagBelow start <&&> isEnemy field

isWhite :: Field -> Bool
isWhite = (White==) . color . flag . good

promotions :: Square -> Piece -> [Piece]
promotions square Pawn | (rankOf square == R8) || (rankOf square == R1) = pieces \\ [Pawn, King]
promotions _     piece | otherwise = [piece] 

castles :: Move.Set -> Field -> [Field]
castles = castlesShort <<++>> castlesLong

data Side = Short | Long

castlesShort :: Move.Set -> Field -> [Field]
castlesShort move field | not (castleShort move) = []
castlesShort move field | (not . short . flag . good) field = []
castlesShort move field | otherwise = castle Short field

castlesLong :: Move.Set -> Field -> [Field]
castlesLong move field | not (castleLong move) = []
castlesLong move field | (not . long . flag . good) field = []
castlesLong move field | otherwise = castle Long field

castle :: Side -> Field -> [Field]
castle side field =
    if 
        any (isThreatened field) [king .. newKing] ||
        any (isOccupied field) (tail [king .. newKing])
    then []
    else
        (:[]) .
        setFlag (updateCastle side) .
        (rook `moveTo` newRook) .
        (king `moveTo` newKing) $ field
    where
    castleFiles Short = (G,H,F)
    castleFiles Long = (C,A,D)
    (g,h,f) = castleFiles side
    king = throneOf field
    rank = rankOf king
    newKing = toSquare (g,rank)
    rook = toSquare(h,rank)
    newRook = toSquare(f,rank)


passants :: Move.Set -> Field -> [Field]
passants move field = do
    soldier <- filter (isPawn <&&> soldierMatch move) $ (soldiers . good) field
    let source = location soldier
    target <- filter (isPassantTarget source <&&> targetMatch move) $ (maybeToList . passant . flag . evil) field  -- encapsulate..?
    return . (source `moveTo` target) . clear (aboveFrom target) $ field
    where
    isPawn = (Pawn==) . authority
    aboveFrom = if (isWhite field) then upFrom else downFrom
    isPassantTarget = if (isWhite field) then isDiagAbove else isDiagBelow

-- TODO
update :: Field -> (Square,Square) -> (Flags -> Flags)
update field (from,to) = id

updateCastle :: Side -> Flags -> Flags
updateCastle Short = updateShort 
updateCastle Long = updateLong 

updateShort :: Flags -> Flags
updateShort = undefined 

updateLong :: Flags -> Flags
updateLong = undefined 

-- TODO
--passant :: Field -> (Square,Square) -> Maybe Square
--passant field (from,to) = Nothing

-- TODO
isThreatened :: Field -> Square -> Bool
isThreatened field square = False 

kingOf :: Field -> Soldier
kingOf = head . filter isKing . soldiers . good
    where isKing = (King==) . authority

throneOf :: Field -> Square
throneOf = location . kingOf

check :: Field -> Bool
check field = isThreatened field (throneOf field)

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
