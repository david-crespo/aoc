module Day15 (part1, part2) where

import qualified Data.Map as M
import qualified Data.Set as S
import Text.Regex.TDFA ((=~), getAllTextMatches)

type Pt = (Int, Int)

readInt :: String -> Int
readInt = read

readLine :: String -> (Pt, Pt)
readLine s = ((a, b), (c, d)) where
  [a, b, c, d] = map readInt $ (getAllTextMatches (s =~ "[0-9-]+") :: [String])

noBeaconPts :: Int -> (Pt, Pt) -> S.Set Pt
noBeaconPts cy ((x0, y0), (a, b)) = if cy - y0 > dist then S.empty
                                    else S.fromList [(x0 + dx, cy) | dx <- [-dist..dist], abs dx + dy <= dist ] where
  dy = abs (cy - y0)
  dist = (abs (b - y0)) + (abs (a - x0))

part1 :: IO ()
part1 = do
  -- input <- readFile "../input/day15.txt"
  input <- readFile "../input/day15-example.txt"
  let sensors = map readLine (lines input)
  let beacons = S.fromList $ map snd sensors
  let noBeacon = S.difference (S.unions $ map (noBeaconPts 2000000) sensors) beacons
  print $ length noBeacon

-------------------------------

-- full grid represented as a map from y coordinates to lists of x ranges of
-- points that could contain a beacon. points are subtracted from ranges with
-- diffRange, and when a y-row has no more points, it is removed altogether.
-- pretty sure this makes the diamond subtraction more efficient because it
-- will ignore any row that has already been eliminated
type Grid = M.Map Int [Pt] 

-- diamond implied by a single sensor + beacon pair, represented as map
-- from y -> (xmin, xmax). don't need a list of x ranges because there's
-- always only one
type Diamond = M.Map Int Pt

diamond :: (Pt, Pt) -> Diamond
diamond ((sx, sy), (bx, by)) = M.fromList [
  let dx = dist - (abs dy) in 
    (sy + dy, (sx - dx, sx + dx)) | dy <- [-dist..dist]
  ] where
  dist = (abs (by - sy)) + (abs (bx - sx))

diffRange :: Pt -> Pt -> [Pt]
diffRange (a, b) (c, d) = if c > b || d < a then [(a, b)]
                          else if c <= a && d >= b then []
                          else if c > a && d < b then [(a, c-1), (d+1, b)]
                          else if c <= a && d < b then [(d+1, b)]
                          else [(a, c - 1)]

subDiamond :: Grid -> Diamond ->  Grid
subDiamond grid dmnd = M.differenceWith diff grid dmnd where
  diff :: [Pt] -> Pt -> Maybe [Pt]
  diff gridRow range = case pts of [] -> Nothing
                                   _ -> Just pts 
                                   where 
    pts = concatMap ((flip diffRange) range) gridRow

part2 :: IO ()
part2 = do
  input <- readFile "../input/day15.txt"
  -- input <- readFile "../input/day15-example.txt"
  let sensors = map (diamond . readLine) (lines input)
  let m = 4000000
  let grid = M.fromList [(y, [(0, m)]) | y <- [0..m]]
  let (y, [(x, _)]) = head $ M.toList $ foldl subDiamond grid sensors
  print $ x * 4000000 + y