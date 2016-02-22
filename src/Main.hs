module Main where

import System.Environment
import qualified Data.List as L
import Grid

-- http://www.gchq.gov.uk/press_and_media/news_and_features/Pages/Directors-Christmas-puzzle-2015.aspx

main :: IO ()
main = do
  mapM_ (printSolutions "Row" . mkLine) rows
  mapM_ (printSolutions "Col" . mkLine) cols where
    printSolutions lbl ln = putStrLn $ lbl ++ " " ++ drawLine ln ++ " has " ++
                            show (length $ solveLine 25 ln) ++ " solutions"

-- Some test data to play with
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

preRows = [
  nW 25,
  nW 25,
  nW 25,
  nW 3 ++ nB 2 ++ nW 7 ++ nB 2 ++ nW 7 ++ nB 1 ++ nW 3,
  nW 25,
  nW 25,
  nW 25,
  nW 25,
  nW 6 ++ nB 2 ++ nW 2 ++ nB 1 ++ nW 3 ++ nB 2 ++ nW 2 ++ nB 1 ++ nW 6,
  nW 25,
  nW 25,
  nW 25,
  nW 25,
  nW 25,
  nW 25,
  nW 25,
  nW 6 ++ nB 1 ++ nW 4 ++ nB 1 ++ nW 4 ++ nB 1 ++ nW 3 ++ nB 1 ++ nW 4,
  nW 25,
  nW 25,
  nW 25,
  nW 25,
  nW 3 ++ nB 2 ++ nW 4 ++ nB 2 ++ nW 4 ++ nB 1 ++ nW 4 ++ nB 2 ++ nW 3,
  nW 25,
  nW 25,
  nW 25]

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

preCols = [
  nW 25,
  nW 25,
  nW 25,
  nW 3 ++ nB 1 ++ nW 17 ++ nB 1 ++ nW 3,
  nW 3 ++ nB 1 ++ nW 17 ++ nB 1 ++ nW 3,
  nW 25,
  nW 8 ++ nB 1 ++ nW 7 ++ nB 1 ++ nW 8,
  nW 8 ++ nB 1 ++ nW 16,
  nW 25,
  nW 21 ++ nB 1 ++ nW 3,
  nW 8 ++ nB 1 ++ nW 12 ++ nB 1 ++ nW 3,
  nW 16 ++ nB 1 ++ nW 8,
  nW 3 ++ nB 1 ++ nW 21,
  nW 3 ++ nB 1 ++ nW 21,
  nW 8 ++ nB 1 ++ nW 16,
  nW 8 ++ nB 1 ++ nW 12 ++ nB 1 ++ nW 3,
  nW 16 ++ nB 1 ++ nW 8,
  nW 25,
  nW 8 ++ nB 1 ++ nW 16,
  nW 25,
  nW 16 ++ nB 1 ++ nW 4 ++ nB 1 ++ nW 3,
  nW 3 ++ nB 1 ++ nW 17 ++ nB 1 ++ nW 3,
  nW 25,
  nW 25,
  nW 25]
