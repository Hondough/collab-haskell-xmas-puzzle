module Grid where

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

blockAt :: Int -> Int -> Grid -> Maybe Block
blockAt row col grid = do
  mRow <- SL.atMay grid row
  SL.atMay mRow col

run :: Int -> Block -> Line
run = replicate

-- turn a list of runs into a Line (list of Blocks)
-- a "run" is an unbroken sequence of black Blocks
mkLine :: [Int] -> Line
mkLine [] = []
mkLine (x:y:xs) = run x B ++ [W] ++ mkLine (y:xs)
mkLine [x] = run x B

freeSpaces :: Int -> Line -> Int
freeSpaces maxLen line = let len = maxLen - length line in
  if len < 0 then 0 else len

moves :: Line -> Int
moves = freeSpaces 25
