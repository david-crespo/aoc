module Day08 (part1, part2) where

import Data.List (inits, findIndex)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import qualified Data.Set as S

type Pt = (Int, Int)
type Grid = M.Map Pt Int

parseGrid :: String -> Grid 
parseGrid s = M.fromList $ concatMap parseLine (zip [0..] (lines s)) where
  parseLine :: (Int, String) -> [(Pt, Int)]
  parseLine (i, line) = map (\(j, c) -> ((i,j), read [c])) (zip [0..] line)

getRow :: Int -> Grid -> [(Pt, Int)]
getRow i g = [((i, j), M.findWithDefault 0 (i, j) g) | j <- [0..maxIdx] ] where
  maxIdx = ((floor . sqrt . fromIntegral) (length g)) - 1

getCol :: Int -> Grid -> [(Pt, Int)]
getCol j g = [((i,j), M.findWithDefault 0 (i, j) g) | i <- [0..maxIdx] ] where
  maxIdx = ((floor . sqrt . fromIntegral) (length g)) - 1

getVisible :: [(Pt, Int)] -> [Pt]
getVisible [] = []
getVisible xs = [fst (head xs)] ++ map (fst . fst) (filter isVisible $ zip xs maxes) where
  maxes = map maximum $ (:) [0] $ tail $ inits $ map snd xs
  isVisible ((_,h),m) = h > m

score :: Grid -> Pt -> Int
score grid (x,y) = product $ map getVisibleCount [up, left, down, right] where
  h = look (x,y)
  getVisibleCount pts = length $ take ((fromMaybe (length pts) (findIndex (>=h) pts)) +1) pts
  maxIdx = ((floor . sqrt . fromIntegral) (length grid)) - 1
  up = map look $ reverse [(i, y) | i <- [0..x-1]]
  down = map look $ [(i, y) | i <- [x+1..maxIdx]]
  left = map look $ reverse [(x, j) | j <- [0..y-1]]
  right = map look $ [(x, j) | j <- [y+1..maxIdx]]
  look = \pt -> M.findWithDefault 0 pt grid

part1 :: IO ()
part1 = do
  input <- readFile "../input/day08.txt"
  -- input <- readFile "../input/day08-example.txt"
  let grid = parseGrid input
  -- print $ grid
  let maxIdx = ((floor . sqrt . fromIntegral) (length grid)) - 1
  let rowsCols = [getRow i grid | i <- [0..maxIdx]] ++ [getCol i grid | i <- [0..maxIdx]]
  let allDirs = rowsCols ++ map reverse rowsCols
  print $ length $ S.fromList $ concatMap getVisible allDirs

part2 :: IO ()
part2 = do
  input <- readFile "../input/day08.txt"
  -- input <- readFile "../input/day08-example.txt"
  let grid = parseGrid input
  let maxIdx = ((floor . sqrt . fromIntegral) (length grid)) - 1
  -- print $ score grid (3,2)
  print $ maximum $ map (score grid) (map fst (M.toList grid))