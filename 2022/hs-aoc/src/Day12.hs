module Day12 (part1, part2) where

import Data.Graph.AStar
import Data.Char (ord)
import Data.List (find, sort)
import Data.Maybe (fromJust, catMaybes)
import qualified Data.Map as M
import qualified Data.HashSet as H

type Pt = (Int, Int)
type Grid = M.Map Pt Char

parse :: String -> [(Pt, Char)]
parse s =  concatMap parseLine (zip [0..] (lines s)) where
  parseLine :: (Int, String) -> [(Pt, Char)]
  parseLine  (y, line) = map (\(x, c) -> ((x,y), c)) (zip [0..] line)

nb :: Grid -> Pt -> [Pt]
nb grid (x, y) = (
  (if x > 0 then [(x-1, y)] else [])
  ++ (if x < xmax then [(x+1, y)] else []) 
  ++ (if y > 0 then [(x, y-1)] else []) 
  ++ (if y < ymax then [(x, y+1)] else []) ) where
  xmax = maximum $ map (fst . fst) (M.toList grid)
  ymax = maximum $ map (snd . fst) (M.toList grid)

arrowsForPt :: Grid -> Pt -> H.HashSet Pt
arrowsForPt grid pt = H.fromList $ concatMap toEdges (nb grid pt) where
  lk = fromJust . ((flip M.lookup) grid)
  currEl = lk pt
  toEdges :: Pt -> [Pt]
  toEdges other = if ord (lk other) - ord currEl <= 1 then [other] else []


getPath :: Grid -> Pt -> Pt -> Maybe [Pt]
getPath grid end start = aStar (arrowsForPt grid) (\_ _ -> 1) (\_ -> 0) (== end) start

part1 :: IO ()
part1 = do
  input <- readFile "../input/day12.txt"
  -- input <- readFile "../input/day12-example.txt"
  let pairs = parse input
  let start = fst $ fromJust $ find (((==) 'S') . snd) pairs
  let end = fst $ fromJust $ find (((==) 'E') . snd) pairs
  let grid = M.insert end 'z' $ M.insert start 'a' $ M.fromList pairs
  print $ length $ fromJust $ getPath grid end start

part2 :: IO ()
part2 = do
  input <- readFile "../input/day12.txt"
  -- input <- readFile "../input/day12-example.txt"
  let pairs = parse input
  let start = fst $ fromJust $ find (((==) 'S') . snd) pairs
  let end = fst $ fromJust $ find (((==) 'E') . snd) pairs
  let grid = M.insert end 'z' $ M.insert start 'a' $ M.fromList pairs
  let starts = map fst $ filter (((==) 'a') . snd) (M.toList grid)
  print $ head $ sort $ map length $ catMaybes $ map (getPath grid end) starts