
module Pgn.Parser (
    parse,
    Result(..),
    Move(..),
    nullmove, stripGlyph,
    MoveTree, trees, result,
    Tag, tag,
    Game, game) where

import Control.Applicative
import Data.Char
import System.IO
import Data.Tree

import Monad.Parser
import Pgn.Glyph

data Result = White | Draw | Black | Unknown 
    deriving (Eq, Ord)

instance Show Result where
    show White = "1-0"
    show Black = "0-1"
    show Draw = "1/2-1/2"
    show Unknown = "*"

data Move = Move {
    precomment :: String,
    move :: String,
    glyph :: Int,
    comment :: String } deriving (Eq)

instance Show Move where
    show x = move x ++ toString (glyph x)

instance Read Move where
    readsPrec _ s = [(Move "" s 0 "","")]

nullmove :: Move
nullmove = read ""

stripGlyph :: Move -> Move
stripGlyph (Move p m 0 c) = let (m',g) = strip m in Move p m' g c 
stripGlyph (Move p m g c) = let (m',_) = strip m in Move p m' g c 

type MoveTree = Tree Move

result = const Unknown

trees :: Parser [MoveTree]
trees = done <|> do
    p <- getComment
    movenum
    m <- getMove
    g <- getGlyph
    c <- getComment
    siblings <- concat <$> many subtrees
    children <- trees
    let me = Node (Move p m g c) children 
    return (me:siblings)

subtrees :: Parser [MoveTree]
subtrees = symbol "(" >> trees

done :: Parser [MoveTree]
done = getResult >> return []

getMove :: Parser String
getMove = token . some $ sat (not.delim)
    where delim c = isSpace c || (c == ')')

getComment :: Parser String
getComment = inside '{' '}' <|> return ""

getGlyph :: Parser Int
getGlyph = (symbol "$" >> natural) <|> return 0

movenum :: Parser ()
movenum = natdots <|> return ()
    where natdots = natural >> some (symbol ".") >> return ()

getResult :: Parser Result
getResult = 
    (symbol (show White) >> return White) <|>
    (symbol (show Black) >> return Black) <|>
    (symbol (show Draw) >> return Draw) <|>
    (symbol (show Unknown) >> return Unknown) <|>
    (symbol ")" >> return Unknown) <|> 
    (end >> return Unknown)

type Tag = (String,String)

tag :: Parser Tag
tag = do
    symbol "["
    name <- word
    val <- quote
    symbol "]"
    return (name,val)

type Game = ([MoveTree],[Tag])

game :: Parser Game
game = do
    tags <- many tag
    ts <- trees
    return (map (fmap stripGlyph) ts,tags)
