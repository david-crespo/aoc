module Day18 (part1, part2) where

import Data.Graph.AStar (aStar)
import qualified Data.HashSet as H
import Data.List (group, sort, (\\), nub)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (isJust)

type Pt = (Int, Int, Int)

type Side = (Pt, Pt)

parseLine :: String -> Pt
parseLine s = (a,b,c) where 
  [a,b,c] = map read $ splitOn "," s

getSides :: Pt -> [Side]
getSides pt@(x,y,z) = map (toSide pt) [(x+1,y,z),(x-1,y,z),(x,y+1,z),(x,y-1,z),(x,y,z+1),(x,y,z-1)] where
  toSide :: Pt -> Pt -> Side
  toSide a b = (c, d) where
    [c, d] = sort [a, b]

part1 :: IO ()
part1 = do
  input <- readFile "../input/day18.txt"
  -- input <- readFile "../input/day18-example.txt"
  let pts = map parseLine (lines input)
  let sides = filter (\g -> (length g) == 1) $ group $ sort $ concatMap getSides pts
  print $ length sides

inBounds :: Int -> Int -> Pt -> Bool
inBounds minv maxv (x,y,z) = minv <= x && x <= maxv && minv <= y && y <= maxv && minv <= z && z <= maxv

getNeighbors :: Pt -> H.HashSet Pt
getNeighbors (x,y,z) = H.fromList $ filter (inBounds (-1) 22) [(x+1,y,z),(x-1,y,z),(x,y+1,z),(x,y-1,z),(x,y,z+1),(x,y,z-1)] 

type Graph = M.Map Pt (H.HashSet Pt)

dist :: Pt -> Pt -> Int
dist (x,y,z) (a,b,c) = abs (x-a) + abs (y-b) + abs (z-c)

getPath :: Graph -> Pt -> Pt -> Maybe [Pt]
getPath grid start end = aStar ((M.!) grid) (\_ _ -> 1) (dist end) (== end) start

isExterior :: Graph -> Pt -> Bool
isExterior grid pt = isJust $ getPath grid (-1,-1,-1) pt

part2 :: IO ()
part2 = do
  input <- readFile "../input/day18.txt"
  -- input <- readFile "../input/day18-example.txt"
  let lava = H.fromList $ map parseLine (lines input)
  let sides = map head $ filter (\g -> (length g) == 1) $ group $ sort $ concatMap getSides lava

  -- wait is this A*? lol. lmao. a side is on the exterior if there's a path to
  -- it from an arbitrary point outside. so you need a graph of pts. they all
  -- start connected, and when a pt has lava in it, you remove it from the graph

  -- min x is 1, min y, z are 0, max x, y, and z are all 21, so just do like -1 to 22 or something

  let gridPts = [(x,y,z) | x <- [-1..22], y <- [-1..22], z <- [-1..22]]

  let grid = M.fromList $ zip gridPts (map getNeighbors gridPts)
  -- remove lava points from keys
  let grid' = M.filterWithKey (\k _ -> not $ H.member k lava) grid
  -- remove lava points from values
  let graph = M.map (\pts -> H.difference pts lava) grid'

  let sidePts = nub $ concatMap (\(a,b) -> [a,b]) sides

  let isExt = isExterior graph
  let ext = H.fromList $ filter isExt sidePts -- slooooooooooooooooooooow
  let extSides = filter (\(a,b) -> H.member a ext || H.member b ext) sides
  print $ length extSides

  -- after succeeding with the above strategy and thinking about how to optimize
  -- it by reducing the huge number of wasted points in empty space, I realized
  -- this is really more of a connected components thing — the exterior is a big
  -- connected component. So you run a CC alg, then you can check any point for
  -- ext by whether it's in the ext CC
  -- https://hackage.haskell.org/package/containers-0.6.6/docs/Data-Graph.html#g:7