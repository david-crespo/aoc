module Day11 (part1, part2) where

import Data.List (sort)

data Monkey = Monkey {
  items :: [Int],
  func :: Int -> Int,
  test :: (Int, Int, Int),
  ct :: Int
}

example = [
  Monkey {
    items = [79, 98],
    func = (*) 19,
    test = (23, 2, 3),
    ct = 0
  },
  Monkey {
    items = [54, 65, 75, 74],
    func = (+) 6,
    test = (19, 2, 0),
    ct = 0
  },
  Monkey {
    items = [79, 60, 97],
    func = floor . (\x -> x ** 2) . fromIntegral,
    test = (13, 1, 3),
    ct = 0
  },
  Monkey {
    items = [74],
    func = (+) 3,
    test = (17, 0, 1),
    ct = 0
  } ]

problem = [
  Monkey {
    items = [92, 73, 86, 83, 65, 51, 55, 93],
    func = (*) 5,
    test = (11, 3, 4),
    ct = 0
  },
  Monkey {
    items = [99, 67, 62, 61, 59, 98],
    func = floor . (\x -> x ** 2) . fromIntegral,
    test = (2,6,7),
    ct = 0
  },
  Monkey {
    items = [81, 89, 56, 61, 99],
    func = (*) 7,
    test = (5,1,5),
    ct = 0
  },
  Monkey {
    items = [97, 74, 68],
    func = (+) 1,
    test = (17,2,5),
    ct = 0
  },
  Monkey {
    items = [78,73],
    func = (+) 3,
    test = (19,2,3),
    ct = 0
  },
  Monkey {
    items = [50],
    func = (+) 5,
    test = (7,1,6),
    ct = 0
  },
  Monkey {
    items = [95, 88, 53, 75],
    func = (+) 8,
    test = (3,0,7),
    ct = 0
  },
  Monkey {
    items = [50, 77, 98, 85, 94, 56, 89],
    func = (+) 2,
    test = (13, 4, 0),
    ct = 0
  } ]

addAt :: [Monkey] -> (Int, Int) -> [Monkey]
addAt monkeys (val, i) = a ++ [m { items = items m ++ [val] } ] ++ b where
  (a, (m:b)) = splitAt i monkeys

clearAt :: [Monkey] -> Int -> Int -> [Monkey]
clearAt monkeys i throwCt = a ++ [m { items = [], ct = ct m + throwCt } ] ++ b where
  (a, (m:b)) = splitAt i monkeys

fst3 :: (a, b, c) -> a
fst3 (x,_,_) = x

doRound :: [Monkey] -> [Monkey]
doRound monkeys = foldl doThrows monkeys [0..length monkeys - 1] where
  doThrows :: [Monkey] -> Int -> [Monkey]
  doThrows ms i = foldl addAt (clearAt ms i (length throws)) throws where
    m = ms !! i
    throws = map testItem (items m)
    testItem :: Int -> (Int, Int)
    testItem item = (newVal, if newVal `mod` td == 0 then tt else tf) where
      (td, tt, tf) = test m
      newVal = ((func m) item) `div` 3

doRound2 :: [Monkey] -> [Monkey]
doRound2 monkeys = foldl doThrows2 monkeys [0..length monkeys - 1] where
  doThrows2 :: [Monkey] -> Int -> [Monkey]
  doThrows2 ms i = foldl addAt (clearAt ms i (length throws)) throws where
    m = ms !! i
    throws = map testItem (items m)
    x = product $ map (fst3 . test) ms
    testItem :: Int -> (Int, Int)
    testItem item = (newVal, if newVal `mod` td == 0 then tt else tf) where
      (td, tt, tf) = test m
      newVal = ((func m) item) `mod` x

monkeyBusiness :: [Monkey] -> Int
monkeyBusiness = product . take 2 . reverse . sort . map ct

part1 :: IO ()
part1 = do
  print $ monkeyBusiness $ (iterate doRound example) !! 20
  print $ monkeyBusiness $ (iterate doRound problem) !! 20

part2 :: IO ()
part2 = do
  print $ monkeyBusiness $ (iterate doRound2 example) !! 10000
  print $ monkeyBusiness $ (iterate doRound2 problem) !! 10000