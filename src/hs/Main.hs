import Trefix
import Data.Char
import System.IO
import System.Environment
import Data.List

type State = (Tree String, Location)

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          args <- getArgs
          if null args 
            then loop ([[]],(0,-1))
          else
            do file <- readFile (args!!0)
               let lns = lines file
               let tree = if null lns then [[]] else map words lns
               loop (tree, (0,-1))

loop :: State -> IO ()
loop s = do putState s
            done <- isEOF
            if done 
                then return ()
            else 
                do ln <- getLine
                   loop (readcmd ln s)

putState :: State -> IO ()
putState s@(t,(h,d)) = do putStr (currentVal s)
                          putStr " "
                          (putStr . show) h
                          putStr " "
                          (putStr . show) d
                          putStr " "
                          (putStrLn . showTree) t

showTree :: Tree String -> String
showTree t = intercalate "]," (map (intercalate ",") t)

currentVal :: State -> String
currentVal (_,(_,-1)) = "-"
currentVal s = (uncurry get) s

readcmd :: String -> (State -> State)
readcmd [] = id
readcmd str = readcmd' cmd args
    where 
        ws = words str
        cmd = head ws
        args = tail ws

readcmd' :: String -> [String] -> (State -> State)
readcmd' "next" _ = uncurry (constTree next)
readcmd' "prev" _ = uncurry (constTree (\_ l -> prev l))
readcmd' "fall" _ = uncurry (constTree fall)
readcmd' "climb" _ = uncurry (constTree climb)
readcmd' "leaf" _ = uncurry (constTree leaf)
readcmd' "root" _ = uncurry (constTree root)
readcmd' "bottom" _ = uncurry (constTree bottom)
readcmd' "top" _ = uncurry (constTree top)
readcmd' "branch" _ = uncurry (constTree branch)
readcmd' "snap" _ = uncurry (constTree snap)
readcmd' "rotateDown" _ = uncurry (constTree rotateDown)
readcmd' "rotateUp" _ = uncurry (constTree rotateUp)
readcmd' "add" (val:_) = uncurry (add val)
readcmd' "chop" _ = uncurry chop
readcmd' "promote" _ = uncurry promote
readcmd' "demote" _ = uncurry demote
readcmd' _ _ = id

