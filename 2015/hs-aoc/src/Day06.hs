module Day06 (part1, part2) where

import Text.ParserCombinators.Parsec
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Either (fromRight)

type Pt = (Int, Int)

data Cmd = 
  On Pt Pt 
  | Toggle Pt Pt
  | Off Pt Pt 
  deriving Show

int :: GenParser Char st Int
int = read <$> many1 digit

pair :: GenParser Char st Pt
pair = (,) <$> int <*> (string "," *> int)

thru :: GenParser Char st String
thru = string " through "

on :: GenParser Char st Cmd
on = On <$> (try (string "turn on ") *> pair) <*> (thru *> pair)

toggle :: GenParser Char st Cmd
toggle = Toggle <$> (try (string "toggle ") *> pair) <*> (thru *> pair)

off :: GenParser Char st Cmd
off = Off <$> (try (string "turn off ") *> pair) <*> (thru *> pair)

line :: GenParser Char st [Cmd]
line = many $ (on <|> toggle <|> off) <* newline

---

pts ::  Pt -> Pt -> S.Set Pt
pts (x1, y1) (x2, y2) = S.fromList [(x, y) | x <- [x1..x2], y <- [y1..y2]]

pts2 ::  Int -> Pt -> Pt -> M.Map Pt Int
pts2 v (x1, y1) (x2, y2) = M.fromList [((x, y), v) | x <- [x1..x2], y <- [y1..y2]]

runCmd ::  S.Set Pt -> Cmd -> S.Set Pt
runCmd grid (On a b) = S.union grid (pts a b)
runCmd grid (Off a b) = S.difference grid (pts a b)
runCmd grid (Toggle a b) = S.union (S.difference grid p) (S.difference p grid) where 
  p = pts a b

addMin0 :: (Ord a, Num a) => a -> a -> a
addMin0 a b = max (a+b) 0
  
runCmd2 ::  M.Map Pt Int -> Cmd -> M.Map Pt Int
runCmd2 grid (On a b) = M.unionWith (+) grid (pts2 1 a b)
runCmd2 grid (Toggle a b) = M.unionWith (+) grid (pts2 2 a b)
runCmd2 grid (Off a b) = M.unionWith addMin0 grid (M.intersection (pts2 (-1) a b) grid)

part1 :: IO ()
part1 = do
  input <- readFile "../input/day06.txt"
  let cmds = fromRight [] $ parse line "(unknown)" input
  print $ length $ foldl runCmd S.empty cmds

part2 :: IO ()
part2 = do
  input <- readFile "../input/day06.txt"
  let cmds = fromRight [] $ parse line "(unknown)" input
  print $ sum $ foldl runCmd2 M.empty cmds
