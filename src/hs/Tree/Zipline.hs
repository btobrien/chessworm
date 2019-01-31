
module Tree.Zipline where

import Data.List

import Utils (equal, pair, replace, limit)

type Tree a = [[a]]
type Height = Int
type Depth = Int
type Location = (Height, Depth)

nulltree = [[]]

start :: (Height, Depth)
start = (0,-1)

constTree :: (Tree a -> Location -> Location) -> (Tree a -> Location -> (Tree a, Location))
constTree f t l = (t,f t l)

constLoc :: (Tree a -> Location -> Tree a) -> (Tree a -> Location -> (Tree a, Location))
constLoc f t l = (f t l, l)

get :: Tree a -> Location -> a
get t (h,d) = t !! h !! d

next :: Eq a => Tree a -> Location -> Location
next t (h,d) = if length (t!!h) > d+1 then (h,d+1) else (h,d)

prev :: Location -> Location
prev (h,d) = if d > 0 then (h,d-1) else (h,-1)

path :: Int -> [a] -> [a]
path = take . (+1)

ancestor :: Eq a => [a] -> [a] -> Int
ancestor xs ys = subtract 1 . length . takeWhile equal $ zip xs ys

sharePathThrough :: Eq a => Int -> [a] -> [a] -> Bool
sharePathThrough d ln1 ln2 = (path d ln1) `isPrefixOf` ln2

highers :: Eq a => Tree a -> Location -> Tree a
highers t (h,d) = takeWhile (not.sharePathThrough d ln) t
    where ln = t !! h

siblings :: Eq a => Tree a -> Location -> Tree a
siblings t (h,d) = takeWhile (sharePathThrough d ln) (dropWhile (not.sharePathThrough d ln) t)
    where ln = t !! h

lowers :: Eq a => Tree a -> Location -> Tree a
lowers t (h,d) = dropWhile (sharePathThrough d ln) (dropWhile (not.sharePathThrough d ln) t)
    where ln = t !! h

cousins :: Eq a => Tree a -> Location -> Tree a
cousins t (h,d) = siblings t (h,d-1)

groupCousinsBySibling :: Eq a => Tree a -> Location -> [Tree a]
groupCousinsBySibling t (h,d) = groupBy (sharePathThrough d) (cousins t (h,d)) 

isOnlyCousin :: Eq a => Tree a -> Location -> Bool
isOnlyCousin t loc = length (groupCousinsBySibling t loc) == 1

append :: Eq a => Tree a -> Height -> a -> Tree a
append t h a = take h t ++ [ln ++ [a]] ++ drop (h+1) t
    where ln = t !! h

height :: Eq a => Tree a -> [a] -> Height
height t ln = length . takeWhile (not . isPrefixOf ln) $ t

traverse :: Eq a => Tree a -> [a] -> Location
traverse t ln = (height t ln, length ln - 1)

result :: Eq a => Tree a -> [a] -> (Tree a, Location)
result t ln = (t, Tree.Zipline.traverse t ln)

lift :: Eq a => Tree a -> Location -> Location
lift t (h,d) = (h,d')
    where 
    ln = t !! h
    highs = highers t (h,d-1)
    lows = lowers t (h,d-1)
    a  = if null highs then [] else [ancestor ln (last highs) + 1]
    a'  = if null lows then [] else [ancestor ln (head lows) + 1]
    candidates = a ++ a'
    d' = if null candidates then 0 else maximum candidates 

slide :: Eq a => Tree a -> Location -> Location
slide t (h,d) = (h,d')
    where 
    ln = t !! h
    sibs = siblings t (h,d)
    a  = ancestor ln (head sibs) + 1
    a' = ancestor ln (last sibs) + 1
    d' = if length sibs == 1 then (length ln - 1) else min a a'

climb :: Eq a => Tree a -> Location -> Location
climb t (h,d) = if (not.null) highs && areCousins then (h',d) else (h,d)
    where 
    ln = t !! h
    highs = highers t (h,d)
    candidate  = last highs
    areCousins = sharePathThrough (d-1) ln candidate
    h' = height t (path d candidate)

fall :: Eq a => Tree a -> Location -> Location
fall t (h,d) = if (not.null) lows && areCousins then (h',d) else (h,d)
    where 
    ln = t !! h
    lows = lowers t (h,d)
    candidate  = head lows
    areCousins = sharePathThrough (d-1) ln candidate
    h' = height t (path d candidate)

snap :: Eq a => Tree a -> Location -> Location
snap _ (h,d) | h <= 0 || d <= 0 = (h,d)
snap t (h,d) = if h == h' then snap t (h,d'-1) else (h',d')
    where (h',d') = climb t . lift t $ (h,d+1)

branch :: Eq a => Tree a -> Location -> Location
branch t (h,d) | h+1 >= length t || d <= 0  = (h,d)
branch t (h,d) = if h == h' then branch t (h,d'-1) else (h',d')
    where (h',d') = fall t . lift t $ (h,d+1)

add :: Eq a => a -> Tree a -> Location -> (Tree a, Location)
add a t (h,d) | d' == length ln         = pair (append t h a) (h,d')
              | otherwise               = result t' ln'
    where
    d' = d + 1
    ln = t !! h
    ln' = path d ln ++ [a]
    sibs = siblings t (h,d)
    t' = if any ((==a).(!!d')) sibs
            then t
            else highers t (h,d) ++ sibs ++ [ln'] ++ lowers t (h,d)

chop :: Eq a => Tree a -> Location -> (Tree a, Location)
chop t (h,d) = result t' ln'
    where
    ln = t !! h
    ln' = take d ln
    onlyCousin = isOnlyCousin t (h,d)
    t' = if (d==0 || not onlyCousin)
            then filter (not.sharePathThrough d ln) t
            else highers t (h,d) ++ [ln'] ++ lowers t (h,d)


promote :: Eq a => Tree a -> Location -> (Tree a, Location)
promote t (h,d) | h == 0    = pair t (h,d)
                | otherwise = result t' ln
    where 
    ln = path d (t!!h)
    a = ancestor (t!!h) (t!!(h-1))
    loc = (h,a+1)
    loc' = (h-1,a+1)
    t' = highers t loc' ++ siblings t loc ++ siblings t loc' ++ lowers t loc

sub :: Eq a => Tree a -> Location -> (Tree a, Location)
sub t (h,d) = pair t' (h',d - d0)
    where
    (_,d0) = lift t (h,d)
    h' = h - length (highers t (h,d))
    t' = map (drop d0) (siblings t (h,d))

rename :: Eq a => a -> Tree a -> Location -> Tree a
rename a t (h,d) = highers t (h,d) ++ sibs ++ lowers t (h,d)
    where sibs = map (replace d a) $ siblings t (h,d)

root :: Tree a -> Location -> Location
root _ (h,_) = (h,0)

leaf :: Eq a => Tree a -> Location -> Location
leaf t (h,_) = (h,d)
    where d = (subtract 1) . length $ (t !! h)

top :: Eq a => Tree a -> Location -> Location
top t = limit (climb t)

bottom :: Eq a => Tree a -> Location -> Location
bottom t = limit (fall t)

mainline :: Eq a => Tree a -> Location -> (Tree a, Location)
mainline t (0,d) = (t,(0,d))
mainline t (h,d) = uncurry mainline $ promote t (h,d)

