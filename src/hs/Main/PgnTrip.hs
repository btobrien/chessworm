
import Pgn.Printer

main = do
    inp <- getContents
    parseGames inp

parseGames inp = case parse game inp of
        Nothing -> return ()
        Just (g,[]) -> putGame g
        Just (g,inp') -> putGame g >> parseGames inp' 

