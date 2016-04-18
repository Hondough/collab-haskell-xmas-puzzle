module Main where

import Grid
import qualified Data.Vector as V
import qualified Data.List as L

-- http://www.gchq.gov.uk/press_and_media/news_and_features/Pages/Directors-Christmas-puzzle-2015.aspx

main :: IO ()
main = do
  let f = solutions $ initialGrid rows cols
  let rowCols = interleave (f DRow rows) (f DCol cols)
  print [(dir (head ld), idx (head ld), length ld) | ld <- rowCols]

-- sln (l:ls) g = [ans | g' <- apply l to g, ans <- sln ls g', l consistent g']

answer :: [Grid]
answer = finalGrid (initialGrid rows cols) allLines



--Repeatedly apply solutions against grid to sift down to an answer
finalGrid :: Grid -> [(LineDir, [Int])] -> [Grid]
finalGrid g [] = g
finalGrid g (l:ls) = [ ans | g' <- applyLineToGrid l g
                            ,ans <- finalGrid g' ls]

--applyLineToGrid :: [Int] -> LineDir -> Grid
applyLineToGrid (DRow, rs) g = fillRow (Row r) $ zipWith (mkLineData DRow) [0] [rs]
--applyLineToGrid cs DCol c g = fillCol col l g where
--  col = Col c
--  l = line $ mkLineData DCol c cs


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
