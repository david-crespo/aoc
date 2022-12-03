module Day05 (part1, part2) where

import Data.List (group, isInfixOf, sort)
import qualified Data.Set as Set

vowels :: Set.Set Char
vowels = Set.fromList "aeiou"

cond1 :: String -> Bool
cond1 s = vowelCount > 2
  where
    vowelCount = length $ filter (flip Set.member vowels) s

cond2 :: String -> Bool
cond2 = any (> 1) . map length . group

cond3 :: String -> Bool
cond3 s = not $ or (map (\p -> isInfixOf p s) ["ab", "cd", "pq", "xy"])

part1 :: IO ()
part1 = do
  input <- readFile "../input/day05.txt"
  print $ length $ filter (\s -> and [cond1 s, cond2 s, cond3 s]) (lines input)

cond4 :: String -> Bool
cond4 s = any (\g -> length g > 1) $ group $ sort $ concatMap (\g -> if length g > 1 then tail g else g) $ group $ zip s (tail s)

cond5 :: String -> Bool
cond5 s = any (\(a, _, c) -> a == c) (zip3 s (tail s) (tail (tail s)))

part2 :: IO ()
part2 = do
  input <- readFile "../input/day05.txt"
  print $ length $ filter (\s -> and [cond4 s, cond5 s]) (lines input)