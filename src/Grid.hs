module Grid where

import qualified Data.Vector as V
import qualified Safe as SL
import Control.Monad

data Block = B | W

type Line = [Block]
type Grid = [Line] --[[Block]]

instance Show Block where
  show B = "1"
  show W = "_"

instance Eq Block where
  B == B = True
  W == W = True
  _ == _ = False

instance Ord Block where
  compare B W = GT
  compare W B = LT
  compare _ _ = EQ

data LineDir = Row | Col deriving Show

data LineData = LineData {
  dir :: LineDir
  ,idx :: Int
  ,locked :: Bool
  ,line :: Line
  ,moves :: Int
} deriving Show

mkLineData :: LineDir -> Int -> [Int] -> LineData
mkLineData direction index runs = LineData {
  dir = direction
  ,idx = index
  ,locked = False
  ,line = mkLine runs
  ,moves = freeSpaces 25 runs
}

-- returns the block at (row,col), if any
readBlock :: Int -> Int -> Grid -> Block
readBlock row col grid = (grid !! row) !! col

writeBlock :: Int -> Int -> Grid -> Grid
writeBlock row col grid = V.toList $ vgrid V.// [(row, V.toList $ vRow V.// [(col, B)])] where
  vgrid = V.fromList grid
  vRow = V.fromList $ vgrid V.! row

-- turn a list of runs into a Line (list of Blocks)
-- a "run" is an unbroken sequence of black Blocks
mkLine :: [Int] -> Line
mkLine [] = []
mkLine (x:y:xs) = run x B ++ [W] ++ mkLine (y:xs)
mkLine [x] = run x B

-- returns the number of free spaces we have to move blocks around within
-- within the max length of a line corresponding to the Int list
freeSpaces :: Int -> [Int] -> Int
freeSpaces maxLen runs = if len < 0 then 0 else len where
  len = maxLen - lineLen
  lineLen = sum runs + length runs - 1

grow :: Int -> Line -> [Line]
grow moves line
  | moves <= 0 = [line]
  | otherwise = line : grow (moves - 1) (W : line)

run :: Int -> Block -> Line
run = replicate
