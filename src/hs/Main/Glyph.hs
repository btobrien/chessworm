
import System.Environment
import System.Exit

import Utils (trim)
import Pgn.Glyph (strip, toInt)

main = do
    args <- getArgs
    move <- fmap trim getContents
    let (move',_) = strip move
    let glyph = head args
    if null args then
        putStrLn move'
    else
        case toInt glyph of
            0 -> exitFailure
            _ -> putStrLn (move' ++ glyph)
        
