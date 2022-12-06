module Day06 (part1, part2) where

import Data.List (zip4, nub, findIndex, transpose)

toList :: (a, a, a, a) -> [a]
toList (a,b,c,d) = [a,b,c,d]

part1 :: IO ()
part1 = do
  input <- readFile "../input/day06.txt"
  let chunks = map toList $ zip4 input (tail input) ((tail . tail) input) ((tail . tail . tail) input)
  print $ fmap (+4) (findIndex (\c -> length (nub c) == 4) chunks)

part2 :: IO ()
part2 = do
  input <- readFile "../input/day06.txt"
  let chunks = filter (\x -> length x == 14) $ transpose $ take 14 $ iterate tail input
  print $ fmap (+14) (findIndex (\c -> length (nub c) == 14) chunks)