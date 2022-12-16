module Day14 (part1, part2) where

import Text.ParserCombinators.Parsec
import Data.Either (fromRight)
import qualified Data.Set as S

type Pt = (Int, Int)

num :: GenParser Char st Int
num = read <$> many1 digit

parsePt :: GenParser Char st Pt
parsePt = (,) <$> (num <* string ",") <*> num

path :: GenParser Char st [Pt]
path = sepBy parsePt (string " -> ")

file :: GenParser Char st [[Pt]]
file = endBy path newline

range :: Int -> Int -> [Int]
range a b = if a < b then [a..b] else [b..a]

-----------------------------------------------

pathToPts :: [Pt] -> S.Set Pt
pathToPts path = S.fromList $ concatMap rangeToPts pairs where
  pairs = zip path (tail path)
  rangeToPts ((x1, y1), (x2, y2)) = [(x,y) | x <- range x1 x2, y <- range y1 y2]

tick :: S.Set Pt -> Pt -> Pt
tick occ pt@(x,y) = if S.notMember down occ then down
                    else if S.notMember left occ then left 
                    else if S.notMember right occ then right else pt where
  down  = (x,   y+1)
  left  = (x-1, y+1)
  right = (x+1, y+1)

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil p = foldr (\x ys -> x : if p x then [] else ys) []

dropGrain :: (S.Set Pt, Bool) -> (S.Set Pt, Bool)
dropGrain (rockAndSand, _) = (if done then rockAndSand else S.insert (x,y) rockAndSand, done) where
  path = iterate (tick rockAndSand) (500, 0)
  ((x,y), (x',y')) = last $ takeUntil (\((_,y1),(_,y2)) -> y2 == y1 || y2 == maxY) $ zip path (tail path)
  done = y' == maxY
  maxY = maximum $ map snd $ S.toList rockAndSand

display :: S.Set Pt -> IO ()
display pts = putStrLn s where
  s = unlines [[ if S.member (x,y) pts then '#' else '.' | x <- [minX..maxX]] | y <- [minY..maxY]]
  ys = S.map snd pts
  xs = S.map fst pts
  (minY, maxY) = (minimum ys - 3, maximum ys + 3)
  (minX, maxX) = (minimum xs - 3, maximum xs + 3)

part1 :: IO ()
part1 = do
  input <- readFile "../input/day14.txt"
  -- input <- readFile "../input/day14-example.txt"
  let paths = fromRight [] $ parse file "(unknown)" input
  let rock = S.unions $ map pathToPts paths
  let (newRock, _) = until snd dropGrain (rock, False)
  print $ length $ S.difference newRock rock

-----------------------------------------------

tick2 :: Int -> S.Set Pt -> Pt -> Pt
tick2 maxY occ pt@(x,y) = if y == maxY then pt
                     else if S.notMember down occ then down
                     else if S.notMember left occ then left 
                     else if S.notMember right occ then right else pt where
  down  = (x,   y+1)
  left  = (x-1, y+1)
  right = (x+1, y+1)

dropGrain2 :: Int -> (S.Set Pt, Bool) -> (S.Set Pt, Bool)
dropGrain2 maxY (rockAndSand, _) = (S.insert (x,y) rockAndSand, done) where
  path = iterate (tick2 (maxY) rockAndSand) (500, 0)
  (x,y) = fst $ last $ takeUntil (\((_,y1),(_,y2)) -> y2 == y1 || y2 == maxY + 1) $ zip path (tail path)
  done = (x,y) == (500, 0)

part2 :: IO ()
part2 = do
  -- input <- readFile "../input/day14.txt"
  input <- readFile "../input/day14-example.txt"
  let paths = fromRight [] $ parse file "(unknown)" input
  let rock = S.unions $ map pathToPts paths
  let maxY = 1 + (maximum $ map snd $ S.toList rock)
  let (newRock, _) = until snd (dropGrain2 maxY) (rock, False)
  display rock
  display newRock
  print $ length $ S.difference newRock rock