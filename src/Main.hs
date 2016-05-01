module Main where

import Grid
import qualified Data.Vector as V
import qualified Data.List as L
import Control.Monad

-- http://www.gchq.gov.uk/press_and_media/news_and_features/Pages/Directors-Christmas-puzzle-2015.aspx

main :: IO ()
main = do




  -- print [(dir (head ld), idx (head ld), length ld) | ld <- rowCols]
  mapM_ print $ head (allSolutions rows cols)

allSolutions :: [[Int]] -> [[Int]] -> [Grid]
allSolutions r c = btAnswer g0 [] rowCols [] where
  g0 = initialGrid r c
  f = solutions g0
  rowCols = interleave (f DRow r) (f DCol c)

-- Helpers for debugging

-- take 2 $ foldr (:) [] [0..]

-- λ let g0 = initialGrid rows cols
-- λ let rc = rowCols rows cols
-- λ mapM_ print $ head $ go [g0] rc
go :: [Grid] -> [[LineData]] -> [Grid]
go = foldr (\l grids -> concatMap (`addCompatible` l) grids)

addCompatible :: Grid -> [LineData] -> [Grid]
addCompatible g = foldr (\l acc -> if compatibleGridLine (rows,cols) g l
                                   then writeLine g l : acc
                                   else acc) []

-- λ let g0 = initialGrid rows cols
-- λ let rc = rowCols rows cols
-- λ mapM_ print $ head $ expandLineData rc [g0]
expandLineData :: [[LineData]] -> [Grid] -> [Grid]
expandLineData [] grids = grids
expandLineData (l:ls) grids = concatMap (\g -> expandLineData ls (expandGrid g l)) grids

expandGrid :: Grid -> [LineData] -> [Grid]
expandGrid grid ls = [writeLine grid l | l <- ls, compatibleGridLine (rows,cols) grid l]

rowCols r c = interleave (f DRow r) (f DCol c) where
  f = solutions g0
  g0 = initialGrid r c

answer3 :: Grid -> [[LineData]] -> [(LineDir, Int)]
answer3 grid [] = []
answer3 grid ([] : more) = []
answer3 grid ((sol:solutions):more) = let newGrid = writeLine grid sol in
  if compatibleGrid grid sol
    then (dir sol, idx sol) : answer3 newGrid more
    else (dir sol, negate (idx sol)): answer3 grid (solutions:more)

f [] acc = acc
f ([]:more) acc = acc
f ((x:xs):more) acc = if even x then f more (x:acc) else f (xs:more) acc


rowCols = interleave (f DRow rows) (f DCol cols)
  where
    f = solutions $ initialGrid rows cols

rawData = zip (repeat DRow) rows ++ zip (repeat DCol) cols


--Repeatedly apply solutions against grid to sift down to an answer
finalGrid :: [[LineData]] -> [Grid] -> [Grid]
finalGrid [] gs = gs
finalGrid (ld:lds) gs  = [ ans | g <- gs
--                              ,ans <- finalGrid lds $ filter (\l -> compatibleLine (line l) $ gridLine g (idx l) (dir l)) $ map (applyLineToGrid g) ld
                              ,ans <- finalGrid lds $ applyHelper g ld
                            ]

applyHelper :: Grid -> [LineData] -> [Grid]
applyHelper g ls = [ applyLineToGrid g l | l <- ls
                        , compatibleLine (line l) (gridLine g (idx l) (dir l))]

applyLineToGrid :: Grid -> LineData -> Grid
applyLineToGrid g ld
  | dir ld == DRow = fillRow (Row $ idx ld) (line ld) g
  | dir ld == DCol = fillCol (Col $ idx ld) (line ld) g

-- let g0 = initialGrid rows cols
-- let as = allSolutions rows cols
-- let rc = rowCols rows cols
-- map (compatibleGrid g0) (head rc)

{-
  Data to initialize the puzzle
  InitialHints is the grid given to us with pre-filled blocks
  InitialGrid is InitialHints with any rows and columns filled in that can only
    fit in one way.
-}
initialHints :: Grid
initialHints = foldr (\(r,c) acc -> fill (Row r) (Col c) acc) blank blackStart where
  blank = V.replicate 25 $ run 25 U

initialGrid :: [[Int]] -> [[Int]] -> Grid
initialGrid rows cols = foldr (\v acc -> fillCol (Col (idx v)) (line v) acc) g0 $ zeros c where
  zeros = filter (\x -> 0 == moves x)
  r = zipWith (mkLineData DRow) [0..] rows
  c = zipWith (mkLineData DCol) [0..] cols
  g0 = foldr (\v acc -> fillRow (Row (idx v)) (line v) acc) initialHints $ zeros r

blackStart :: [(Int, Int)]
blackStart = [(3,3), (3,4), (3,12), (3,13), (3,21)
  , (8,6), (8,7), (8,10), (8,14), (8,15), (8,18)
  , (16,6), (16,11), (16,16), (16,20)
  , (21,3), (21,4), (21,9), (21,10), (21,15), (21,20), (21,21)]

allLines :: [(LineDir, [Int])]
allLines = interleave (zip (repeat DRow) rows) (zip (repeat DCol) cols)

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
