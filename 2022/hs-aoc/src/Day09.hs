module Day09 (part1, part2) where

import Data.List (nub)

type Pt = (Int, Int)

-- Convert "R 4\nU 2\n" into "RRRRUU"
parseMoves :: String -> String
parseMoves = concatMap (\[d, n] -> replicate ((read n)::Int) (head d)) . map words . lines

moveH :: Pt -> Char -> Pt
moveH (x,y) 'U'  = (x, y+1)
moveH (x,y) 'R' = (x+1, y)
moveH (x,y) 'D' = (x, y-1)
moveH (x,y) 'L' = (x-1, y)
moveH pt _ = pt

moveT :: Pt -> Pt -> Pt
moveT (hx,hy) (tx,ty) = if abs dx == 2 || abs dy == 2
                          then (tx + signum dx, ty + signum dy)
                        else (tx, ty) where
  dx = hx - tx
  dy = hy - ty
  -- after doing it the long way below, I figured out it was equiv to above one-liner
  -- moveT' :: Pt -> Pt
  -- moveT' (2,0) = (x+1, y)
  -- moveT' (-2,0) = (x-1, y)
  -- moveT' (0,2) = (x, y+1)
  -- moveT' (0,-2) = (x, y-1)
  -- moveT' (2,1) = (x+1, y+1)
  -- moveT' (2,-1) = (x+1, y-1)
  -- moveT' (1,2) = (x+1, y+1)
  -- moveT' (1,-2) = (x+1, y-1)
  -- moveT' (-2,1) = (x-1, y+1)
  -- moveT' (-2,-1) = (x-1, y-1)
  -- moveT' (-1,2) = (x-1, y+1)
  -- moveT' (-1,-2) = (x-1, y-1)

  -- -- the hint about new kinds of moves in part 2 meant adding this
  -- moveT' (2,2) = (x+1, y+1)
  -- moveT' (2,-2) = (x+1, y-1)
  -- moveT' (-2,2) = (x-1, y+1)
  -- moveT' (-2,-2) = (x-1, y-1)
  -- moveT' _ = t

move :: (Pt, Pt) -> Char -> (Pt, Pt)
move (h, t) d = (newH, moveT newH t) where
  newH = moveH h d

move2 :: Char -> [Pt] -> [Pt]
move2 _ [] = []
move2 d pts = foldl red [] pts where
  red :: [Pt] -> Pt -> [Pt]
  red [] h = [moveH h d]
  red newPts oldPt = newPts ++ [moveT (last newPts) oldPt]

part1 :: IO ()
part1 = do
  input <- readFile "../input/day09.txt"
  -- input <- readFile "../input/day09-example.txt"
  let moves = parseMoves input
  let pts = reverse $ foldl (\ps d -> (move (head ps) d):ps) [((0,0),(0,0))] moves
  let tpts = nub $ map snd pts
  print $ length tpts


part2 :: IO ()
part2 = do
  input <- readFile "../input/day09.txt"
  -- input <- readFile "../input/day09-example.txt"
  let moves = parseMoves input
  let initial = replicate 10 (0,0)
  let path = reverse $ foldl (\ps d -> (move2 d (head ps)):ps) [initial] moves
  print $ length $ nub $ map (!! 9) path