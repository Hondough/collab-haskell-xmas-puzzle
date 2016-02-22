module Grid
{-
(
  Square(B,W)
  ,Line
  ,drawLine
  ,mkLine
  ,solveLine
)
-}
where

import qualified Data.List as L
import qualified Data.List.Extra as LX
import qualified Safe as SL
import Data.Maybe

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
-- a "run" is an unbroken sequence of black Squares
mkLine :: [Int] -> Line
mkLine [] = []
mkLine (x:y:xs) = replicate x B ++ [W] ++ mkLine (y:xs)
mkLine [x] = replicate x B

-- grow a line by a single white block in all available slots
-- you can add a white at the head, on the end and wherever there
-- is another white block. all runs of black blocks are maintained.
inc :: Line -> [Line]
inc xs = L.foldl' addWS [] indices ++ [xs ++ [W]] where
  addWS acc n = (take n xs ++ [W] ++ drop n xs) : acc
  indices = leadingEdge False 0 [] xs
  -- find indices for the start of each "run"
  leadingEdge _ _ acc [] = acc
  leadingEdge inRun idx acc (x:xs)
    | x == B = leadingEdge True (idx+1) (if not inRun then idx:acc else acc) xs
    | otherwise = leadingEdge False (idx+1) acc xs

rmDupInc :: [Line] -> [Line]
rmDupInc = LX.nubOrd . concatMap inc

consistent :: Line -> Line -> Bool
consistent ln mustBlack = and $ zipWith (\a b -> a == W || a == b) ln mustBlack

-- create a list of solutions by growing the previous list of solutions
-- until we reach the desired length for a solution line
solveLine :: Int -> Line -> [Line]
solveLine len ln = solutions [ln] where
  solutions [[]] = [[]]
  solutions xs
    | length (head xs) >= len = xs
    | otherwise = solutions $ rmDupInc xs

nB :: Int -> Line
nB = flip replicate B

nW :: Int -> Line
nW = flip replicate W
