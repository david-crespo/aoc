module Day01
  ( part1,
    part2,
  )
where

import Data.List (sort)
import Data.List.Split (splitOn)

readInt :: String -> Int
readInt = read

sums :: String -> [Int]
sums input = map (sum . map readInt . lines) $ splitOn "\n\n" input

part1 :: IO ()
part1 = do
  input <- readFile "../input/day01.txt"
  print $ foldl max 0 (sums input)

part2 :: IO ()
part2 = do
  input <- readFile "../input/day01.txt"
  print $ sum $ take 3 $ reverse $ sort (sums input)