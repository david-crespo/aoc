module Day20 (part1, part2) where

import qualified Data.CircularList as C
import Data.Maybe (fromJust)

-- (original index, value)
type Entry = (Int, Int)
type Ring = C.CList Entry

-- move currently focused item
move :: Ring -> Ring
move ring = if value == 0 then ring 
             else if value > 0 then C.insertL e $ C.rotNR amt $ C.removeR ring
             else C.insertR e $ C.rotNL amt $ C.removeL ring where
  e@(_, value) = fromJust $ C.focus ring
  -- without this mod it blows up to 10s of gigs of RAM
  amt = ((abs value) - 1) `mod` (length ring - 1)

focusIndex :: Int -> Ring -> Ring
focusIndex i ring = fromJust $ C.findRotateTo ((== i) . fst) ring

mix :: Ring -> Ring
mix ring = snd $ iterate exec (0, ring) !! length ring where
  exec :: (Int, Ring) -> (Int, Ring)
  exec (i, r) = (i + 1, move $ focusIndex i r)

part1 :: IO ()
part1 = do
  input <- readFile "../input/day20.txt"
  -- input <- readFile "../input/day20-example.txt"
  let nums = (map read (lines input) :: [Int])
  let ring = C.fromList $ zip [0..] nums
  let result = mix ring
  let fromZero = C.toInfList $ fromJust $ C.findRotateTo ((== 0) . snd) result
  print $ sum $ map (snd . (!!) fromZero) [1000, 2000, 3000]

part2 :: IO ()
part2 = do
  input <- readFile "../input/day20.txt"
  -- input <- readFile "../input/day20-example.txt"
  let nums = (map (((*) 811589153) . read) (lines input) :: [Int])
  let ring = C.fromList $ zip [0..] nums
  let result = iterate mix ring !! 10
  let fromZero = C.toInfList $ fromJust $ C.findRotateTo ((== 0) . snd) result
  print $ sum $ map (snd . (!!) fromZero) [1000, 2000, 3000]