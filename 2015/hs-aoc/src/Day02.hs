module Day02 (part1, part2) where

import Data.List (sort)
import Data.List.Split (splitOn)

readInt :: String -> Int
readInt = read

paper :: (Num a, Ord a) => a -> a -> a -> a
paper l w h = 
  2*l*w + 2*w*h + 2*h*l + s1  * s2 where
    sorted = sort [l,w,h]
    s1 = sorted !! 0
    s2 = sorted !! 1

part1 :: IO ()
part1 = do
  input <- readFile "../input/day02.txt"
  let tuples = map (map readInt . splitOn "x") (lines input)
  print $ sum $ map (\[x,y,z] -> paper x y z) tuples

ribbon :: (Num a, Ord a) => a -> a -> a -> a
ribbon l w h = 
  2*s1 + 2*s2 + l*w*h where
    sorted = sort [l,w,h]
    s1 = sorted !! 0
    s2 = sorted !! 1

part2 :: IO ()
part2 = do
  input <- readFile "../input/day02.txt"
  let tuples = map (map readInt . splitOn "x") (lines input)
  print $ sum $ map (\[x,y,z] -> ribbon x y z) tuples