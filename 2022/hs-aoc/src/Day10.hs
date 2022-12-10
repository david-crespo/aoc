module Day10 (part1, part2) where

import Data.List.Split (chunksOf)

data Op = Noop | Addx Int deriving (Show, Eq)

parseOp :: String -> Op
parseOp "noop" = Noop
parseOp s = (Addx . read . (!! 1) . words) s

type State = (Int, Int) -- (X, cycle)

run :: [State] -> Op -> [State]
run states op = states ++ run' op where
  (x, c) = last states
  run' Noop = [(x, c+1)]
  run' (Addx a) = [(x, c+1), (x+a, c+2)]

draw :: State -> Char
draw (x, c) = if abs (((c `mod` 40)-1)-x) <= 1 then '#' else '.'

part1 :: IO ()
part1 = do
  input <- readFile "../input/day10.txt"
  -- input <- readFile "../input/day10-example.txt"
  let cmds = map parseOp $ lines input
  let cycles = foldl run [(1, 1)] cmds
  let justVals = map fst cycles
  print $ sum $ map (\i -> i * (justVals !! (i-1))) [20,60..220]

part2 :: IO ()
part2 = do
  input <- readFile "../input/day10.txt"
  -- input <- readFile "../input/day10-example.txt"
  let cmds = map parseOp $ lines input
  let cycles = foldl run [(1, 1)] cmds
  putStrLn $ unlines $ chunksOf 40 $ map draw $ cycles