module Day10 (part1, part2) where

import Data.List.Split (chunksOf)

data Op = Noop | Addx Int deriving (Show, Eq)

parseOp :: String -> Op
parseOp "noop" = Noop
parseOp s = Addx $ read $ words s !! 1

type State = (Int, Int) -- (X, cycle)

run :: State -> Op -> State
run (x, c) Noop = (x, c + 1)
run (x, c) (Addx a)  = (x + a, c + 2)

getCycles :: [Op] -> [State]
getCycles cmds = concatMap fillIn (zip states (tail states)) where
  states = scanl run (1, 1) (cmds ++ [Noop])
  -- fill in gaps where the adds were taking two cycles
  fillIn ((ax, ac),(_, bc)) = if bc - ac == 2 then [(ax, ac),(ax, ac+1)] else [(ax, ac)]

draw :: State -> Char
draw (x, c) = if abs (((c `mod` 40)-1)-x) <= 1 then '#' else '.'

part1 :: IO ()
part1 = do
  input <- readFile "../input/day10.txt"
  -- input <- readFile "../input/day10-example.txt"
  let cmds = map parseOp $ lines input
  let justVals = map fst (getCycles cmds)
  print $ sum $ map (\i -> i * (justVals !! (i-1))) [20,60..220]

part2 :: IO ()
part2 = do
  input <- readFile "../input/day10.txt"
  -- input <- readFile "../input/day10-example.txt"
  let cmds = map parseOp $ lines input
  putStrLn $ unlines $ chunksOf 40 $ map draw $ getCycles cmds