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

inc' :: Line -> [Line]
inc' xs = L.foldl' addNoDup [W : xs, xs ++ [W]] (blanks xs) where
  blanks = L.elemIndices W
  addNoDup t v = let newStr = take v xs ++ [W] ++ drop v xs in
    if newStr `elem` t then t else newStr : t

rmDupInc = L.nub . concatMap inc'

solveLine :: Int -> Line -> [Line]
solveLine len ln = solutions [ln] where
  solutions [[]] = [[]]
  solutions xs
    | length (head xs) >= len = xs
    | otherwise = solutions $ rmDupInc xs

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
