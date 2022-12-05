module Day08 (part1, part2) where

import Text.Regex.TDFA ((=~), getAllTextMatches)

    -- [a,b,c] = map readInt (getAllTextMatches (line =~ "[0-9]+") :: [String])

score :: String -> Int
score line =  (length slashes) + (length quotes) + ((length hex) * 3) + 2 where
  slashes = (getAllTextMatches (line =~ "\\\\\\\\") :: [String])
  quotes = (getAllTextMatches (line =~ "\\\\\"") :: [String])
  hex = (getAllTextMatches (line =~ "\\\\x") :: [String])

part1 :: IO ()
part1 = do
  input <- readFile "../input/day08.txt"
  -- print $ lines input
  print $ sum $ map score $ lines input

-- 1387 too high

part2 :: IO ()
part2 = do
  input <- readFile "../input/day08.txt"
  print $ "part2"