module Day12 (part1, part2) where

import Text.Regex.TDFA ((=~), getAllTextMatches)

readInt :: String -> Int
readInt = read

part1 :: IO ()
part1 = do
  input <- readFile "../input/day12.txt"
  print $ sum $ map readInt $ (getAllTextMatches (input =~ "[-0-9]+") :: [String])

part2 :: IO ()
part2 = do
  input <- readFile "../input/day12.txt"
  print $ "part2"