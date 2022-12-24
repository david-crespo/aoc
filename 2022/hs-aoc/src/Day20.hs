module Day20 (part1, part2) where

data Node = Both Int Node Node
          | RightOnly Int Node

toRing :: [Int] -> Node
toRing [] = error "hi"
toRing (n:ns) = RightOnly n (toRing ns)

part1 :: IO ()
part1 = do
  -- input <- readFile "../input/day20.txt"
  input <- readFile "../input/day20-example.txt"
  let nums = (map read (lines input) :: [Int])
  print $ nums

part2 :: IO ()
part2 = do
  -- input <- readFile "../input/day20.txt"
  -- input <- readFile "../input/day20-example.txt"
  print $ "part2"