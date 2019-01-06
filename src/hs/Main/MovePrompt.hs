
import System.Environment (getArgs)
import Pgn.Printer (showNum)

main = do
    (arg:_) <- getArgs
    putStrLn . showNum . (+1) . read $ arg 



