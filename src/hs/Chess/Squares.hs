
module Chess.Squares where
import Utils
import Control.Applicative

data Square =
    A1 | B1 | C1 | D1 | E1 | F1 | G1 | H1 |
    A2 | B2 | C2 | D2 | E2 | F2 | G2 | H2 |
    A3 | B3 | C3 | D3 | E3 | F3 | G3 | H3 |
    A4 | B4 | C4 | D4 | E4 | F4 | G4 | H4 |
    A5 | B5 | C5 | D5 | E5 | F5 | G5 | H5 |
    A6 | B6 | C6 | D6 | E6 | F6 | G6 | H6 |
    A7 | B7 | C7 | D7 | E7 | F7 | G7 | H7 |
    A8 | B8 | C8 | D8 | E8 | F8 | G8 | H8
        deriving (Enum, Eq, Ord)

dimension :: Int
dimension = 8

data File = A | B | C | D | E | F | G | H
    deriving (Show, Enum, Eq)

data Rank = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8
    deriving (Show, Enum, Eq)

squares :: [Square]
squares = everyone

files :: [File]
files = everyone

ranks :: [Rank]
ranks = everyone

toFile :: Char -> Maybe File
toFile 'a' = Just A
toFile 'b' = Just B
toFile 'c' = Just C
toFile 'd' = Just D
toFile 'e' = Just E
toFile 'f' = Just F
toFile 'g' = Just G
toFile 'h' = Just H
toFile _ = Nothing

toRank :: Char -> Maybe Rank
toRank '1' = Just R1
toRank '2' = Just R2
toRank '3' = Just R3
toRank '4' = Just R4
toRank '5' = Just R5
toRank '6' = Just R6
toRank '7' = Just R7
toRank '8' = Just R8
toRank _ = Nothing

fileOf :: Square -> File
fileOf = toEnum . (`mod` dimension) . fromEnum

rankOf :: Square -> Rank
rankOf = toEnum . (`div` dimension) . fromEnum

toSquare :: (File,Rank) -> Square
toSquare (f,r) = toEnum $ x + (dimension * y)
    where
    x = fromEnum f
    y = fromEnum r

tryRead :: String -> Maybe Square
tryRead = undefined

instance Show Square where
    show A8 = "a8"
    show B8 = "b8"
    show C8 = "c8"
    show D8 = "d8"
    show E8 = "e8"
    show F8 = "f8"
    show G8 = "g8"
    show H8 = "h8"
    show A7 = "a7"
    show B7 = "b7"
    show C7 = "c7"
    show D7 = "d7"
    show E7 = "e7"
    show F7 = "f7"
    show G7 = "g7"
    show H7 = "h7"
    show A6 = "a6"
    show B6 = "b6"
    show C6 = "c6"
    show D6 = "d6"
    show E6 = "e6"
    show F6 = "f6"
    show G6 = "g6"
    show H6 = "h6"
    show A5 = "a5"
    show B5 = "b5"
    show C5 = "c5"
    show D5 = "d5"
    show E5 = "e5"
    show F5 = "f5"
    show G5 = "g5"
    show H5 = "h5"
    show A4 = "a4"
    show B4 = "b4"
    show C4 = "c4"
    show D4 = "d4"
    show E4 = "e4"
    show F4 = "f4"
    show G4 = "g4"
    show H4 = "h4"
    show A3 = "a3"
    show B3 = "b3"
    show C3 = "c3"
    show D3 = "d3"
    show E3 = "e3"
    show F3 = "f3"
    show G3 = "g3"
    show H3 = "h3"
    show A2 = "a2"
    show B2 = "b2"
    show C2 = "c2"
    show D2 = "d2"
    show E2 = "e2"
    show F2 = "f2"
    show G2 = "g2"
    show H2 = "h2"
    show A1 = "a1"
    show B1 = "b1"
    show C1 = "c1"
    show D1 = "d1"
    show E1 = "e1"
    show F1 = "f1"
    show G1 = "g1"
    show H1 = "h1"

