
import qualified Chess.Board as Board
import Chess.Move

newtype Position = Position Board.Board

initial :: Position
initial = Position Board.initial

move :: String -> Position -> Position
move = undefined
