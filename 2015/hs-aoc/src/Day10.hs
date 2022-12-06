module Day10 (part1, part2) where

import Data.List (group)

input :: String
input = "1113222113"

looksay :: String -> String
looksay = concat . map (\g -> show (length g) ++ take 1 g) . group 

part1 :: IO ()
part1 = do
  print $ length $ iterate looksay input !! 40

part2 :: IO ()
part2 = do
  print $ length $ iterate looksay input !! 50