module Day23 (part1, part2) where

import Data.List (findIndex, intercalate, nub)
import qualified Data.Map as M
import qualified Data.HashSet as H
import Data.Maybe (mapMaybe, listToMaybe, isJust)

type Pt = (Int, Int)
type Grid = H.HashSet Pt

data Dir = N | S | W | E deriving (Show, Eq)

viz :: Int -> Int -> Int -> Int -> Grid -> IO ()
viz xmin xmax ymin ymax grid = putStrLn (intercalate "\n" rows ++ "\n") where
  rows = [ [if H.member (x, y) grid then '#' else '.' | x <- [xmin..xmax] ] | y <- [ymin..ymax] ]

vizDyn :: Grid -> IO ()
vizDyn grid = viz xmin xmax ymin ymax grid where
  xs = H.map fst grid
  padding = 3
  xmin = (minimum xs) - padding
  xmax = (maximum xs) + padding

  ys = H.map snd grid
  ymin = (minimum ys) - padding
  ymax = (maximum ys) + padding

parseGrid :: String -> Grid
parseGrid input = H.unions $ map (uncurry parseLine) (zip [0..] (lines input)) where
  parseLine :: Int -> String -> Grid
  parseLine y line = H.fromList $ mapMaybe (\(x, c) -> if c == '#' then Just (x,y) else Nothing) (zip [0..] line)

move :: Dir -> Pt -> Pt
move N (x,y) = (x, y-1)
move S (x,y) = (x, y+1)
move W (x,y) = (x-1, y)
move E (x,y) = (x+1, y)

proposeDir :: Grid -> Pt -> Dir -> Maybe Pt
proposeDir grid pt d = if all (\p -> not (H.member p grid)) (nb d pt) then Just (move d pt) else Nothing

nb :: Dir -> Pt -> [Pt]
nb N (x, y) = [ (x-1, y-1), (x, y-1), (x+1, y-1) ]
nb S (x, y) = [ (x-1, y+1), (x, y+1), (x+1, y+1) ]
nb W (x, y) = [ (x-1, y-1), (x-1, y), (x-1, y+1) ]
nb E (x, y) = [ (x+1, y-1), (x+1, y), (x+1, y+1) ]

nb8 :: Pt -> [Pt]
nb8 pt = nub (nb N pt ++ nb S pt ++ nb W pt ++ nb E pt)

rot :: [a] -> [a]
rot [] = []
rot (x:xs) = xs ++ [x]


propose :: Grid -> [Dir] -> Pt -> Maybe Pt
propose grid dirs pt = if any ((flip H.member) grid) (nb8 pt) then listToMaybe $ mapMaybe (proposeDir grid pt) dirs else Nothing

step :: (Grid, [Dir]) -> (Grid, [Dir])
step (grid, dirs) = (H.union adds (H.difference grid deletes), rot dirs) where
  proposalsBySource = map (\(pt, Just prop) -> (pt, prop)) $ filter (\(_, prop) -> isJust prop) $ map (\pt -> (pt, propose grid dirs pt)) (H.toList grid)
  proposalsByTarget = foldl (\acc (src, target) -> M.insertWith (++) target [src] acc) M.empty proposalsBySource

  goodProposals = map (\(target,src) -> (head src, target)) $ M.toList $ M.filter ((== 1) . length) proposalsByTarget

  deletes = H.fromList $ map fst goodProposals
  adds = H.fromList $ map snd goodProposals

emptySpace :: Grid -> Int
emptySpace grid = (xmax + 1 - xmin) * (ymax + 1 - ymin) - length grid where
  xs = H.map fst grid
  xmin = (minimum xs)
  xmax = (maximum xs)

  ys = H.map snd grid
  ymin = (minimum ys)
  ymax = (maximum ys)

part1 :: IO ()
part1 = do
  input <- readFile "../input/day23.txt"
  -- input <- readFile "../input/day23-example.txt"
  -- input <- readFile "../input/day23-example0.txt"
  let grid = parseGrid input 
  -- vizDyn grid
  -- putStrLn ""
  let dirs = [N, S, W, E]
  let (grid', _) = (iterate step (grid, dirs)) !! 10
  -- vizDyn grid'
  print $ emptySpace grid'

part2 :: IO ()
part2 = do
  input <- readFile "../input/day23.txt"
  -- input <- readFile "../input/day23-example.txt"
  -- input <- readFile "../input/day23-example0.txt"
  let grid = parseGrid input 
  let dirs = [N, S, W, E]
  let steps = map fst $ iterate step (grid, dirs)
  let pairs = zip steps (tail steps)
  print $ fmap (+1) $ findIndex (\(a,b) -> a == b) pairs
  