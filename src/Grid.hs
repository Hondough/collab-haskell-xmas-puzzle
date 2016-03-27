module Grid where

import qualified Data.Vector as V
import qualified Safe as SL
import Control.Monad

data Block = B | W | U

type Row = Int
type Col = Int
type Offset = Int
type Run = Int
type Line = V.Vector Block
type Grid = V.Vector Line --[[Block]]

instance Show Block where
  show B = "1"
  show W = "_"
  show U = " "

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

instance Eq LineData where
  (==) a b = moves a == moves b

instance Ord LineData where
  compare a b = compare (moves a) (moves b)

mkLineData :: LineDir -> Int -> [Run] -> LineData
mkLineData direction index runs = LineData {
  dir = direction
  ,idx = index
  ,locked = False
  ,line = mkLine runs
  ,moves = freeSpaces 25 runs
}

-- check if a block to be placed is compatible with the block on the grid
compatibleBlock :: Block -> Block -> Bool
compatibleBlock _ U = True
compatibleBlock block gridBlock = block == gridBlock

-- check if the line of blocks to be place is compatible wtih the line of
-- blocks on the grid
compatibleLine :: Line -> Line -> Bool
compatibleLine ln gridBlocks = and $ V.zipWith compatibleBlock ln gridBlocks

-- returns the block at (row,col)
readBlock :: Row -> Col -> Grid -> Block
readBlock row col grid = (grid V.! row) V.! col

writeBlock :: Block -> Row -> Col -> Grid -> Grid
writeBlock block row col grid = grid V.// [(row, vRow V.// [(col, block)])] where
  vRow = grid V.! row

fill :: Row -> Col -> Grid -> Grid
fill = writeBlock B

erase :: Row -> Col -> Grid -> Grid
erase = writeBlock W

fillRow :: Row -> Line -> Grid -> Grid
fillRow row line grid = grid V.// [(row, line)]

fillCol :: Col -> Line -> Grid -> Grid
fillCol col line grid = foldr (\v acc -> writeBlock (snd v) (fst v) col acc) grid coords where
  coords = zip [0..] (V.toList line)

-- turn a list of runs into a Line (list of Blocks)
-- a "run" is an unbroken sequence of black Blocks
mkLine :: [Run] -> Line
mkLine [] = V.empty
mkLine [x] = run x B
mkLine (x:xs) = run x B V.++ V.singleton W V.++ mkLine xs

-- returns the number of free spaces we have to move blocks around within
-- within the max length of a line corresponding to the Int list
freeSpaces :: Int -> [Run] -> Int
freeSpaces maxLen runs = if len < 0 then 0 else len where
  len = maxLen - lineLen
  lineLen = sum runs + length runs - 1

run :: Int -> Block -> Line
run = V.replicate

curry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
curry3 f (a,b,c) = f a b c

-- recursively expands the input until we have no more runs or free spaces left
expand :: (Line, [Run], Int) -> [(Line, [Run], Int)]
expand (line, [], 0) = [(line, [], 0)]
expand (line, [], free) = [(line V.++ run free W, [], 0)]
expand (line, x:xs, free) = concatMap (expand . paste line) (expandRun x free) where
  paste oldLine (line, free)
    | V.null oldLine = (line, xs, free)
    | otherwise = (oldLine V.++ V.singleton W V.++ line, xs, free)

expandRun :: Run -> Int -> [(Line, Int)]
expandRun r free = [(run n W V.++ run r B, free - n) | n <- [0..free]]
