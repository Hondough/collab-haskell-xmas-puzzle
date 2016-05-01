module Grid where

import qualified Data.Vector as V
import qualified Safe as SL
import Control.Monad

data Block = B | W | U

newtype Row = Row {getRow :: Int} deriving Show
newtype Col = Col {getCol :: Int} deriving Show
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

data LineDir = DRow | DCol deriving (Show, Eq)

data LineData = LineData {
  dir :: LineDir
  ,idx :: Int
  ,runs :: [Int]
  ,line :: Line
  ,moves :: Int
} deriving Show

instance Eq LineData where
  (==) a b = idx a == idx b && dir a == dir b

instance Ord LineData where
  compare a b = compare (idx a) (idx b)

mkLineData :: LineDir -> Int -> [Run] -> LineData
mkLineData direction index runs = LineData {
  dir = direction
  ,idx = index
  ,line = mkLine runs
  ,moves = freeSpaces 25 runs
}

-- check if a block to be placed is compatible with the block on the grid
compatibleBlock :: Block -> Block -> Bool
compatibleBlock _ U = True
compatibleBlock U _ = True
compatibleBlock block gridBlock = block == gridBlock

-- check if the line of blocks to be place is compatible wtih the line of
-- blocks on the grid
compatibleLine :: Line -> Line -> Bool
compatibleLine ln gridBlocks = and $ V.zipWith compatibleBlock ln gridBlocks

compatibleGrid :: Grid -> LineData -> Bool
compatibleGrid grid lineDir = compatibleLine (line lineDir) (gridLine grid lineDir) where
  gridLine grid ld | dir ld == DRow = gridRow grid $ Row (idx ld)
                   | otherwise = gridCol grid $ Col (idx ld)

compatibleGridLine :: ([[Run]],[[Run]]) -> Grid -> LineData -> Bool
compatibleGridLine runs grid ld = hasSolution slns where
 slns = compatibleSolutions grid (dir ld) runs
 hasSolution xs = not $ null xs

compatibleSolutions :: Grid -> LineDir -> ([[Run]],[[Run]]) -> [[LineData]]
compatibleSolutions grid dir rows = map (lnData dir) $
 zip [0..] (lineSolutions (lineBuilder dir grid) (d dir rows)) where
   lineBuilder DCol = gridColList
   lineBuilder DRow = gridRowList
   d DRow = fst
   d _ = snd

gridRow :: Grid -> Row -> Line
gridRow grid row = grid V.! getRow row

gridRowList :: Grid -> [Line]
gridRowList = V.toList

gridCol :: Grid -> Col -> Line
gridCol grid col = V.map (\l -> l V.! getCol col) grid

gridColList :: Grid -> [Line]
gridColList grid = map (gridCol grid . Col) [0..(V.length grid - 1)]

-- returns the block at (row,col)
readBlock :: Row -> Col -> Grid -> Block
readBlock row col grid = (grid V.! getRow row) V.! getCol col

writeBlock :: Block -> Row -> Col -> Grid -> Grid
writeBlock block row col grid = grid V.// [(getRow row, vRow V.// [(getCol col, block)])] where
  vRow = grid V.! getRow row

fill :: Row -> Col -> Grid -> Grid
fill = writeBlock B

erase :: Row -> Col -> Grid -> Grid
erase = writeBlock W

fillRow :: Row -> Line -> Grid -> Grid
fillRow row line grid = grid V.// [(getRow row, line)]

fillCol :: Col -> Line -> Grid -> Grid
fillCol col line grid = foldr (\v acc -> writeBlock (snd v) (fst v) col acc) grid coords where
  coords = zip (map Row [0..]) (V.toList line)

writeLine :: Grid -> LineData -> Grid
writeLine grid ld | dir ld == DRow = fillRow (Row $ idx ld) (line ld) grid
                  | otherwise = fillCol (Col $ idx ld) (line ld) grid

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

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (a,b,c) = f a b c

foo :: a -> b -> c -> d
foo a b c = undefined

interleave :: [a] -> [a] -> [a]
interleave [] _ = []
interleave _ [] = []
interleave (x:xs) ys = x : interleave ys xs

-- recursively expands the input until we have no more runs or free spaces left
expandLine :: Line -> (Line, [Run], Int) -> [(Line, [Run], Int)]
expandLine _ (line, [], 0) = [(line, [], 0)]
expandLine _ (line, [], free) = [(line V.++ run free W, [], 0)]
expandLine gridLine (line, x:xs, free) = concatMap (expandLine gridLine . paste line)
  $ filter compatible (expandRun x free) where
    compatible (x, _) = compatibleLine (pasteLine line x) gridLine
    compatibleAlways _ = True
    paste oldLine (newLine, free) = (pasteLine oldLine newLine, xs, free)
    pasteLine oldLine newLine
      | V.null oldLine = newLine
      | otherwise = oldLine V.++ V.singleton W V.++ newLine

expandRun :: Run -> Int -> [(Line, Int)]
expandRun r free = [(run n W V.++ run r B, free - n) | n <- [0..free]]

-- Generate solutions
answer :: Grid -> [[LineData]] -> [Grid] -> [Grid]
answer grid [] acc = acc
answer grid ([] : more) acc = acc
answer grid ((sol:solutions):more) acc = let newGrid = writeLine grid sol in
  if compatibleGrid grid sol
    then answer newGrid more (newGrid:acc)
    else answer grid (solutions:more) acc

btAnswer :: Grid -> [[LineData]] -> [[LineData]] -> [Grid] -> [Grid]
btAnswer _ _ [] acc = acc
btAnswer grid pop ([] : more) acc = btAnswer grid pop pop acc
btAnswer grid pop ((sol:solutions):more) acc = let newGrid = writeLine grid sol in
  if compatibleGrid grid sol
    then btAnswer newGrid (solutions:more) more (newGrid:acc)
    else btAnswer grid (solutions:more) (solutions:more) acc

solutions :: Grid -> LineDir -> [[Run]] -> [[LineData]]
solutions grid dir rows = map (lnData dir) $
  zip [0..] (lineSolutions (lineBuilder dir grid) rows) where
    lineBuilder DCol = gridColList
    lineBuilder DRow = gridRowList

lnData :: LineDir -> (Int, [Line]) -> [LineData]
lnData direction (index, lns) =
  map (\ln -> LineData {
        dir = direction
        ,idx = index
        ,line = ln
        ,moves = 0
        })
      lns

lineSolutions :: [Line] -> [[Run]] -> [[Line]]
lineSolutions lns runs = map (map (\(x,_,_) -> x)) $ zipWith expandRuns lns runs where
  expandRuns gridLine runs = expandLine gridLine
                                        (V.empty, runs, freeSpaces 25 runs)
