module Day03 (part1, part2) where

import Data.List (nub)

move :: (Int, Int) -> Char -> (Int, Int)
move (x,y) '^' = (x, y+1)
move (x,y) '>' = (x+1, y)
move (x,y) 'v' = (x, y-1)
move (x,y) '<' = (x-1, y)
move (x,y) _ = (x, y)

path :: String -> [(Int, Int)]
path = foldl (\acc c -> (move (head acc) c) : acc) [(0,0)]

part1 :: IO ()
part1 = do
  input <- readFile "../input/day03.txt"
  print $ length $ nub $ path input

part2 :: IO ()
part2 = do
  input <- readFile "../input/day03.txt"
  let od = [ x | (x,y) <- zip input (cycle [True, False]), y]
  let ev = [ x | (x,y) <- zip input (cycle [False, True]), y]
  print $ length $ nub $ (path od ++ path ev)