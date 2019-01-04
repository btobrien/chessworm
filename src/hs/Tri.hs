import Trefix
import Data.Char
import System.IO
import System.Environment
import Data.List
import Control.Monad

type State = (Tree String, Location)

main :: IO ()
main = do 
    hSetBuffering stdout NoBuffering
    inp <- getContents
    tree <- getTree
    mapM_ putState $ treeScan (tree,start) inp

getTree :: IO (Tree String)
getTree = do
    args <- getArgs
    if null args then return nulltree else getFileTree (head args)

getFileTree :: String -> IO (Tree String)
getFileTree fname = do
    file <- readFile fname
    let ls = lines file
    return $ if null ls then nulltree else map words ls

treeScan :: State -> String -> [State]
treeScan s inp = scanl nextState s (lines inp)
    where nextState s ln = readcmd ln s

putState :: State -> IO ()
putState s@(t,(h,d)) = do 
    putStr.currentVal $ s
    putStr " "
    putStr.show $ h
    putStr " "
    putStr.show $ d
    putStr " "
    putStrLn.showTree $ t

showTree :: Tree String -> String
showTree = intercalate "]," . map (intercalate ",")

currentVal :: State -> String
currentVal (_,(_,-1)) = "-"
currentVal s = uncurry get $ s

readcmd :: String -> (State -> State)
readcmd [] = id
readcmd str = readargs cmd args
    where ([cmd],args) = splitAt 1 (words str)

move = uncurry . constTree

readargs :: String -> [String] -> (State -> State)
readargs "next" _ = move next
readargs "prev" _ = move (\_ l -> prev l)
readargs "fall" _ = move fall
readargs "climb" _ = move climb
readargs "leaf" _ = move leaf
readargs "root" _ = move root
readargs "bottom" _ = move bottom
readargs "top" _ = move top
readargs "branch" _ = move branch
readargs "snap" _ = move snap
readargs "rotateDown" _ = move rotateDown
readargs "rotateUp" _ = move rotateUp
readargs "add" (val:_) = uncurry (add val)
readargs "chop" _ = uncurry chop
readargs "promote" _ = uncurry promote
readargs "demote" _ = uncurry demote
readargs _ _ = id

