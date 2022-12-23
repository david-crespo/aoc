module Day16 (part1, part2) where

import Data.Graph.AStar
import qualified Data.HashSet as H 
import Data.List (groupBy)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Text.Regex.TDFA ((=~), getAllTextMatches)

parseLine :: String -> (String, (Int, H.HashSet String))
parseLine s = (src, (rate, H.fromList targets)) where
  (src:targets) = getAllTextMatches (s =~ "[A-Z]{2}")
  rate = read $ head $ getAllTextMatches (s =~ "[0-9]+")

type Edges = M.Map String (H.HashSet String)
type Rates = M.Map String Int

search :: M.Map (String, String) Int -> Rates -> H.HashSet String -> H.HashSet String -> Int -> Int
search _ _ _ _ 0 = 0
search weights rates off on stepsLeft = 0


dist :: Edges -> (String, String) -> Int
dist grid (start, end) = length path where
  path = fromJust $ aStar (\k -> grid M.! k) (\_ _ -> 1) (\_ -> 0) (== end) start

part1 :: IO ()
part1 = do
  -- input <- readFile "../input/day16.txt"
  input <- readFile "../input/day16-example.txt"
  let dat = map parseLine $ lines input
  let rates = M.fromList $ zip (map fst dat) (map (fst.snd) dat)
  let edges = M.fromList $ zip (map fst dat) (map (snd.snd) dat)
  -- have to include AA because it's the starting point
  let workingValves = ["AA"] ++ (M.keys $ M.filter (>0) rates)
  let pairs = [(a, b) | a <- workingValves, b <- workingValves, a /= b]
  let weights = M.fromList $ zip pairs (map (dist edges) pairs)
  -- let newEdges = M.fromList $ map (\ws -> (fst $ fst $ head ws, H.fromList (map (snd . fst) ws))) $ groupBy (\((a,_),_) ((b, _), _) -> a == b) (M.toList weights)
  print $ search weights rates (H.fromList workingValves) H.empty 30

part2 :: IO ()
part2 = do
  -- input <- readFile "../input/day16.txt"
  -- input <- readFile "../input/day16-example.txt"
  print $ "part2"