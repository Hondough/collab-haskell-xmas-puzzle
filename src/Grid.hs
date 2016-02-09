module Grid
-- (
--   Square(B,W)
--   ,drawLine
--   ,mkLine
--   ,inc
--   ,rows
--   ,cols
-- )
where

import qualified Data.List as L

-- a Square can be either black or white
data Square = B | W

-- make it easier to see
instance Show Square where
  show B = "1"
  show W = "_"

instance Eq Square where
  B == B = True
  W == W = True
  _ == _ = False

instance Ord Square where
  compare W B = GT
  compare B W = LT
  compare _ _ = EQ

-- just a helper to simplify code a bit
type Line = [Square]

drawLine :: Line -> String
drawLine = foldl (\t v -> t ++ show v) ""

-- turn a list of runs into a Line (list of Squares)
-- a run is an unbroken sequence of black Squares
mkLine :: [Int] -> Line
mkLine [] = []
mkLine (x:y:xs) = replicate x B ++ [W] ++ mkLine (y:xs)
mkLine [x] = replicate x B

-- grow a line by a single white block in all available slots
-- you can add a white at the head, on the end and wherever there
-- is another white block. all runs of black blocks are maintained.
inc :: Line -> [Line]
inc xs = [W : xs] ++  fill xs ++ [xs ++ [W]] where
  fill xs = L.foldl' (\t v -> (take v xs ++ [W] ++ drop v xs) : t) [] (blanks xs)
  blanks = L.elemIndices W

inc5 :: Line -> [Line]
inc5 [] = [[W]]
inc5 [x] = if x == W then [[W,W]] else [[W,x], [x,W]]
inc5 [W,W] = [[W,W,W]]
inc5 [x,y] = [[W,x,y], [x,y,W]]
inc5 xs = bookEnds xs ++ L.foldl' (\t v -> concat (addW v groups) : t) [] slots where
  groups = L.group xs
  slots = findSlots groups
  addW n xs = take n xs ++  [W:(xs !! n)] ++ drop (n+1) xs

findSlots :: [Line] -> [Int]
findSlots groups = searchLine groups 0 [] where
  searchLine [] _ acc = acc
  searchLine (l:ls) n acc
    | head l == W = searchLine ls (n+1) (n:acc)
    | otherwise = searchLine ls (n+1) acc

bookEnds xs = filter (not . null) [lStart, lEnd] where
  lStart = if head xs == B then W : xs else []
  lEnd = if last xs == B then xs ++ [W] else []

solveLine :: Int -> Line -> [Line]
solveLine n ln = solutions n [ln] where
  solutions _ [[]] = [[]]
  solutions len xs
    | length (head xs) >= len = xs
    | otherwise = solutions len (concatMap inc5 xs)

solveLine' :: Int -> Line -> [Line]
solveLine' n ln = last $ takeWhile (\x -> length (last x) <= n) $
  iterate (concatMap inc5) [ln]

rowLines = map mkLine rows
colLines = map mkLine cols

foo :: Line
foo = mkLine [2,1] -- [B,B,W,B]
bar :: Line
bar = [W,W,B,W,B,B,W,W]

rows :: [[Int]]
rows = [
  [7,3,1,1,7],
  [1,1,2,2,1,1],
  [1,3,1,3,1,1,3,1],
  [1,3,1,1,6,1,3,1],
  [1,3,1,5,2,1,3,1],
  [1,1,2,1,1],
  [7,1,1,1,1,1,7],
  [3,3],
  [1,2,3,1,1,3,1,1,2],
  [1,1,3,2,1,1],
  [4,1,4,2,1,2],
  [1,1,1,1,1,4,1,3],
  [2,1,1,1,2,5],
  [3,2,2,6,3,1],
  [1,9,1,1,2,1],
  [2,1,2,2,3,1],
  [3,1,1,1,1,5,1],
  [1,2,2,5],
  [7,1,2,1,1,1,3],
  [1,1,2,1,2,2,1],
  [1,3,1,4,5,1],
  [1,3,1,3,10,2],
  [1,3,1,1,6,6],
  [1,1,2,1,1,2],
  [7,2,1,2,5]]

cols :: [[Int]]
cols = [
  [7,2,1,1,7],
  [1,1,2,2,1,1],
  [1,3,1,3,1,3,1,3,1],
  [1,3,1,1,5,1,3,1],
  [1,3,1,1,4,1,3,1],
  [1,1,1,2,1,1],
  [7,1,1,1,1,1,7],
  [1,1,3],
  [2,1,2,1,8,2,1],
  [2,2,1,2,1,1,1,2],
  [1,7,3,2,1],
  [1,2,3,1,1,1,1,1],
  [4,1,1,2,6],
  [3,3,1,1,1,3,1],
  [1,2,5,2,2],
  [2,2,1,1,1,1,1,2,1],
  [1,3,3,2,1,8,1],
  [6,2,1],
  [7,1,4,1,1,3],
  [1,1,1,1,4],
  [1,3,1,3,7,1],
  [1,3,1,1,1,2,1,1,4],
  [1,3,1,4,3,3],
  [1,1,2,2,2,6,1],
  [7,1,3,2,1,1]]
