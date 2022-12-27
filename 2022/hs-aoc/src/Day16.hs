module Day16 (part1, part2) where

import Data.Graph.AStar
import qualified Data.HashSet as H 
import Data.List (groupBy, sort)
import qualified Data.HashMap.Strict as M
import Data.Maybe (fromJust)
import Text.Regex.TDFA ((=~), getAllTextMatches)

parseLine :: String -> (String, (Int, H.HashSet String))
parseLine s = (src, (rate, H.fromList targets)) where
  (src:targets) = getAllTextMatches (s =~ "[A-Z]{2}")
  rate = read $ head $ getAllTextMatches (s =~ "[0-9]+")

type Edges = M.HashMap String (H.HashSet String)
type Rates = M.HashMap String Int

search :: M.HashMap String (H.HashSet (String, Int)) -> Rates -> H.HashSet String -> H.HashSet String -> String -> Int -> Int
search _ _ _ _ _ 0 = 0
search edges rates off on curr stepsLeft = if H.null off || H.null options 
                                           then stepsLeft * rate
                                           else maximum $ H.map runOption options where
  rate = sum $ H.map (rates M.!) on
  options = H.filter (\(_,w) -> w <= stepsLeft) $ edges M.! curr
  runOption (n, w) = (rate * w) + (search edges rates (H.delete n off) (H.insert n on) n (stepsLeft - w))


dist :: Edges -> (String, String) -> Int
dist grid (start, end) = length path where
  path = fromJust $ aStar (\k -> grid M.! k) (\_ _ -> 1) (\_ -> 0) (== end) start

part1 :: IO ()
part1 = do
  input <- readFile "../input/day16.txt"
  -- input <- readFile "../input/day16-example.txt"
  let dat = map parseLine $ lines input
  let rates = M.fromList $ zip (map fst dat) (map (fst.snd) dat)
  let edges = M.fromList $ zip (map fst dat) (map (snd.snd) dat)
  -- have to include AA because it's the starting point
  let workingValves = ["AA"] ++ (M.keys $ M.filter (>0) rates)
  let pairs = [(a, b) | a <- workingValves, b <- workingValves, a /= b]
  let weights = M.fromList $ zip pairs (map (dist edges) pairs)
  putStrLn $ unlines $ map (\((a,b),w) -> a ++ " " ++ b ++ " " ++ show w) $ M.toList weights
  putStrLn $ unlines $ map (\(a,r) -> a ++ " " ++ show r) $ M.toList rates
  -- let newEdges = M.fromList $ map (\ws -> (fst $ fst $ head ws, H.fromList (map (\((_,b),w) -> (b, w) ) ws))) $ groupBy (\((a,_),_) ((b, _), _) -> a == b) (sort (M.toList weights))
  -- print $ search newEdges rates (H.fromList workingValves) H.empty "AA" 5
  -- print newEdges

part2 :: IO ()
part2 = do
  -- input <- readFile "../input/day16.txt"
  -- input <- readFile "../input/day16-example.txt"
  print $ "part2"