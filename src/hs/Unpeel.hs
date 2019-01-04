
import PgnParser
import PgnPrinter
import Data.List

unpeel :: Eq a => b -> [[a]] -> Tree b a
unpeel r [[]] = Leaf r
unpeel r ls = Node val children
    where
    val = head . head $ ls
    children = map (unpeel r) . groupBy samenext . map tail $ ls
    
samenext x y = head x == head y

readmove m = Move "" m 0 ""

main = do
    inp <- getContents
    let tree = map (map readmove) $ map words $ lines inp
    putTrees . map (unpeel Unknown) . groupBy samenext $ tree
    putStrLn "\b*"




