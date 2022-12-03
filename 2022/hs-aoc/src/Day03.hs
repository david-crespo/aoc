module Day03 (part1, part2) where

import Data.Maybe (fromJust)
import Data.List (intersect, elemIndex)
import Data.List.Split (chunksOf)

priority :: Char -> Int
priority c = (fromJust $ elemIndex c alphabet) + 1 where
  alphabet = ['a'..'z'] ++ ['A'..'Z']

score :: String -> Int
score s = priority $ head (intersect a b) where
  (a, b) = splitAt ((length s) `div` 2) s

part1 :: IO ()
part1 = do
  input <- readFile "../input/day03.txt"
  print $ sum $ map score (lines input)

score3 :: [String] -> Int
score3 [a,b,c] = priority $ head (a `intersect` b `intersect` c)
score3 _ = 0

part2 :: IO ()
part2 = do
  input <- readFile "../input/day03.txt"
  let groups = chunksOf 3 (lines input)
  print $ sum $ map score3 groups