module Day05 (part1, part2) where

import Data.List (group, isInfixOf)
import qualified Data.Set as Set

vowels :: Set.Set Char
vowels = Set.fromList "aeiou"

cond1 :: String -> Bool
cond1 s = vowelCount > 2 where
  vowelCount = length $ filter (flip Set.member vowels) s

cond2 :: String -> Bool
cond2 = any (>1) . map length . group

cond3 :: String -> Bool
cond3 s = not $ or (map (\p -> isInfixOf p s) ["ab", "cd", "pq", "xy"])

cond :: String -> Bool
cond s = and [cond1 s, cond2 s, cond3 s]

part1 :: IO ()
part1 = do
  input <- readFile "../input/day05.txt"
  print $ length $ filter cond (lines input)

part2 :: IO ()
part2 = do
  input <- readFile "../input/day05.txt"
  print $ "part2"