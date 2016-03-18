module Main where

import Grid
import qualified Data.Vector as V
import qualified Data.List as L

-- http://www.gchq.gov.uk/press_and_media/news_and_features/Pages/Directors-Christmas-puzzle-2015.aspx

main :: IO ()
main = do
  let zeros = filter (\x -> 0 == moves x)
  let r = zipWith (mkLineData Row) [0..24] rows
  let c = zipWith (mkLineData Col) [0..24] cols
  mapM_ print $ zeros r ++ zeros c
  mapM_ print initial
  print "filled"
  let newGrid = foldr (\v acc -> fillRow (idx v) (line v) acc) initial $ zeros r
  mapM_ print $ foldr (\v acc -> fillCol (idx v) (line v) acc) newGrid $ zeros c
  -- mapM_ print $ L.sort r
  -- mapM_ print $ L.sort c

blackStart :: [(Int, Int)]
blackStart = [(3,3), (3,4), (3,12), (3,13), (3,21)
  , (8,6), (8,7), (8,10), (8,14), (8,15), (8,18)
  , (16,6), (16,11), (16,16), (16,20)
  , (21,3), (21,4), (21,9), (21,10), (21,15), (21,20), (21,21)]

initial :: Grid
initial = foldr (\(r,c) acc -> fill r c acc) blank blackStart where
  blank = V.replicate 25 $ run 25 U

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
