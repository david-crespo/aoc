module Day01 (part1, part2) where

import Data.List (inits, elemIndex)

score :: [Char] -> Int
score xs = sum $ map (\x -> if x == '(' then 1 else -1) xs

part1 :: IO ()
part1 = do
  input <- readFile "../input/day01.txt"
  print $ score input


part2 :: IO ()
part2 = do
  input <- readFile "../input/day01.txt"
  print $ elemIndex (-1) $ map score (inits input)
