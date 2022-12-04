module Day04 (part1, part2) where

import Data.List.Split (splitOn)
import Data.List (intersect)

-- import Text.Regex.TDFA ((=~), getAllTextMatches)

readInt :: String -> Int
readInt = read

parseRange = map readInt . splitOn "-"

subsume :: String -> Bool
subsume s = i == length a || i == length b where
  [[a1, a2], [b1, b2]] = map parseRange $ splitOn "," s
  a = [a1..a2]
  b = [b1..b2]
  i = length (intersect a b)

overlap :: String -> Bool
overlap s = i > 0 where
  [[a1, a2], [b1, b2]] = map parseRange $ splitOn "," s
  a = [a1..a2]
  b = [b1..b2]
  i = length (intersect a b)

part1 :: IO ()
part1 = do
  input <- readFile "../input/day04.txt"
  print $ length $ filter subsume $ lines input

  -- print $ ("24-91,80-92" =~ "([0-9]+)-([0-9]+),([0-9]+)-([0-9]+)" :: (String, String, String, [String]))
  -- print $ map readInt (getAllTextMatches ("24-91,80-92" =~ "[0-9]+") :: [String])

part2 :: IO ()
part2 = do
  input <- readFile "../input/day04.txt"
  print $ length $ filter overlap $ lines input