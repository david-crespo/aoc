module Day09 (part1, part2) where

import Data.List (intercalate, nub)
import Text.Regex.TDFA ((=~), getAllTextMatches)

readInt :: String -> Int
readInt = read

pp :: Show a => [a] -> IO ()
pp xs = putStrLn $ intercalate "\n" $ map show xs

parseLine :: String -> (String, String, Int)
parseLine line = (from, to, readInt cost) where
  (a,b,c,d) = line =~ "([^ ]+) to ([^ ]+) = ([0-9]+)" :: (String, String, String, [String])
  [from, to, cost] = d

part1 :: IO ()
part1 = do
  input <- readFile "../input/day09.txt"
  let edges = map parseLine (lines input)
  let cities = nub $ map (\(f,t,c) -> f) edges
  print cities

part2 :: IO ()
part2 = do
  input <- readFile "../input/day09.txt"
  print $ "part2"