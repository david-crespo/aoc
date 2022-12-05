module Day05 (part1, part2) where

import Data.List.Split (splitOn)
import Text.Regex.TDFA ((=~), getAllTextMatches)

readInt :: String -> Int
readInt = read

--                 [M]     [W] [M]    
--             [L] [Q] [S] [C] [R]    
--             [Q] [F] [F] [T] [N] [S]
--     [N]     [V] [V] [H] [L] [J] [D]
--     [D] [D] [W] [P] [G] [R] [D] [F]
-- [T] [T] [M] [G] [G] [Q] [N] [W] [L]
-- [Z] [H] [F] [J] [D] [Z] [S] [H] [Q]
-- [B] [V] [B] [T] [W] [V] [Z] [Z] [M]
--  1   2   3   4   5   6   7   8   9 

initStacks = 
  [ "TZB"
  , "NDTHV"
  , "DMFB"
  , "LQVWGJT"
  , "MQFVPGDW"
  , "SFHGQZV"
  , "WCTLRNSZ"
  , "MRNJDWHZ"
  , "SDFLQM" ]


parseMove :: String -> (Int, Int, Int)
parseMove line = (a,b,c) where
  [a,b,c] = map readInt (getAllTextMatches (line =~ "[0-9]+") :: [String])

replaceAt :: Int -> a -> [a] -> [a]
replaceAt i x xs = as ++ (x:bs) where
  (as, (b:bs)) = splitAt i xs

move :: [String] -> (Int, Int, Int) -> [String]
move s (ct, fromIdx0, toIdx0) = replaceAt toIdx newTo (replaceAt fromIdx newFrom s) where
  fromIdx = fromIdx0 - 1
  toIdx = toIdx0 - 1
  (fromA, newFrom) = splitAt ct (s !! fromIdx)
  to = s !! toIdx
  newTo = (reverse fromA) ++ to

move2 :: [String] -> (Int, Int, Int) -> [String]
move2 s (ct, fromIdx0, toIdx0) = replaceAt toIdx newTo (replaceAt fromIdx newFrom s) where
  fromIdx = fromIdx0 - 1
  toIdx = toIdx0 - 1
  (fromA, newFrom) = splitAt ct (s !! fromIdx)
  to = s !! toIdx
  newTo = fromA ++ to -- only difference is no reverse!

part1 :: IO ()
part1 = do
  input <- readFile "../input/day05.txt"
  let [_, b] = splitOn "\n\n" input
  let cmds = map parseMove (lines b)
  print $ map head $ foldl move initStacks cmds

part2 :: IO ()
part2 = do
  input <- readFile "../input/day05.txt"
  let [_, b] = splitOn "\n\n" input
  let cmds = map parseMove (lines b)
  print $ map head $ foldl move2 initStacks cmds