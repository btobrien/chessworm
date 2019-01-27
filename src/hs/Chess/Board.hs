
module Chess.Board where

import qualified Chess.Squares as Square
import qualified Chess.Move as Move
import qualified Chess.Sides as Side
import qualified Chess.Castling as Castling
import qualified Chess.Battle as Battle

type Move = Move.Move

data Soldier = S {
    side :: Side.Name,
    piece :: Piece.Name,
    location :: Square.Location }

data Army = A {
    king :: Soldier,
    warriors :: [Soldier],
    flags :: Castling.Flags }

data Board = B {
    good :: Army,
    evil :: Army,
    passant :: Square.Location }

type Star = (Square.Location,[Square.Location])
type Constellation = (Piece.Name,[Star])

source = fst

genConstellations :: Board -> [Constellation]
genConstellations brd = do
    n <- Piece.names
    return $ pair n (stars n brd)

genStars :: Piece.Name -> Board -> [Star]
genStars n brd = do
    soldier <- filter ((n==).piece) $ (soldiers.good) brd
    return $ (location soldier, choices soldier brd)
    
genLandings :: Soldier -> Board -> [Square.Location]
genLandings soldier brd = undefined

move :: String -> Board -> [Board]
move inp brd = postMove $ do
    m <- Move.parse inp
    case m of
        CastleShort -> tryCastleShort brd
        CastleLong  -> tryCastleLong brd
        _           -> tryMove m brd

tryCastleShort :: Board -> [Board]
tryCastleShort = undefined

tryCastleLong :: Board -> [Board]
tryCastleShort = undefined

tryMove :: Move -> Board -> [Board]
tryMove (Move p range target promotion) brd = do
    stars <- fmap snd $ filter ((p==).fst) (genConstellations brd)
    (origin,landings) <- filter ((within range).source) stars
    filter (==target) landings
    let s = Soldier (who brd) (greaterOf p promotion) target
    return $ replace origin s brd

postMove :: [Board] -> [Board]
postMove = (map flip) . (filter (not.check))

replace :: Square.Location -> Soldier -> Board -> Board
replace = undefined

new :: Board
new = B (newArmy Side.White) (newArmy Side.Black)

newArmy :: Side.Name -> Army
newArmy Side.White = A (S Side.White Piece.King Square.E1) []
newArmy Side.Black = A (S Side.Black Piece.King Square.E8) []

soldiers :: Army -> [Soldier]
soldiers a = (king a) : (warriors a)

flip :: Battle -> Battle
flip b = B (evil b) (good b)

new :: Board
new = B Battle.new Castling.new Square.Null

who :: Board -> Side.Name
who = undefined

check :: Board -> Bool
check = undefined

mate :: Board -> Bool
mate = undefined
