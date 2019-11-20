import Data.Char
import System.IO
import System.Environment
import Data.List
import Control.Monad
import Control.Applicative
import Data.Maybe
import Data.List.Split (splitOn)

import Tree.Zipline

type State = (Tree String, Location)

main = do 
    hSetBuffering stdout NoBuffering
    filename <- listToMaybe <$> getArgs
    original <- fromMaybe (return nulltree) (getFileTree <$> filename)
    trees <- treeScan (original,start) . map (splitOn ";") . lines <$> getContents
    mapM_ (putState.view) trees
    fromMaybe (return ()) $ flip writeFile (showTreeFile.fst.last$trees) <$> filename

getFileTree :: String -> IO (Tree String)
getFileTree fname = do
    file <- readFile fname
    let ls = lines file
    return $ if null ls then nulltree else map words ls

treeScan :: State -> [[String]] -> [State]
treeScan = scanl nextState
    where nextState = flip readcmds

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
showTree t = intercalate "]," . map (intercalate ",") $ t

showTreeFile :: Tree String -> String
showTreeFile = unlines . map (intercalate " ")

currentVal :: State -> String
currentVal (_,(_,-1)) = "-"
currentVal s = uncurry get s

readcmds :: [String] -> (State -> State)
readcmds = foldr (flip (.)) id . map readcmd

readcmd :: String -> (State -> State)
readcmd [] = id
readcmd str = readargs cmd args
    where ([cmd],args) = splitAt 1 (words str)

readargs :: String -> [String] -> (State -> State)
readargs "next" _ = move next
readargs "prev" _ = move (\_ l -> prev l)
readargs "slide" _ = move slide . lean
readargs "lift" _ = move lift
readargs "leaf" _ = move leaf
readargs "root" _ = move root
readargs "bottom" _ = move bottom
readargs "top" _ = move top
readargs "fall" _ = move fall
readargs "climb" _ = move climb
readargs "add" (val:_) = uncurry (add val)
readargs "chop" _ = uncurry chop
readargs "promote" _ = uncurry promote
readargs "mainline" _ = uncurry mainline
readargs "rename" (val:_) = modify (rename (const val))
readargs "snap" _ = move snap
readargs "branch" _ = move branch
readargs "fold" _ = modify (rename togglefold)
readargs "foldAll" _ = modTree $ (map.map) fold
readargs "unfoldAll" _ = modTree $ (map.map) unfold
readargs "unfoldDown" _ = mapAfter unfold
readargs _ _ = id

move = uncurry . constTree
modify = uncurry . constLoc
modTree f (t,l) = (f t,l)

view = lean . modTree (trim isfold hidefold) . mapBefore unfold

isfold = ('.'==) . head
fold xs = if isfold xs then xs else '.':xs
unfold xs = if isfold xs then tail xs else xs
togglefold xs = if isfold xs then tail xs else '.':xs
hidefold xs = if isfold xs then "." else xs

