module Day07 (part1, part2) where

import Data.List (sort)
import Data.List.Split (splitOn)

data Node = Dir String [Node] | File String Int
  deriving Show

data Line = Cd String | Ls | DirL String | FileL String Int
  deriving Show

nodeSize :: Node -> Int
nodeSize (File _ size) = size
nodeSize (Dir _ children) = sum $ map nodeSize children

listDirs :: Node -> [Node]
listDirs node = listDirs' [] node where
  listDirs' ns curr@(Dir _ children) = (curr:ns) ++ concatMap listDirs children 
  listDirs' ns _ = ns

readInt :: String -> Int
readInt = read

parseFile :: String -> Line
parseFile s = FileL name (readInt size) where
  [size, name] = splitOn " " s

parse :: String -> Line
parse s = if take 4 s == "$ ls" 
            then Ls
          else if take 4 s == "$ cd"
            then Cd (drop 5 s)
          else if take 4 s == "dir "
            then DirL (drop 4 s)
          else parseFile s

acc :: ([Node], Node) -> Line -> ([Node], Node)
acc (ps, curr) Ls = (ps, curr)
acc ([], curr) (Cd name') = ([curr], Dir name' [])
acc (p@(Dir pname pchildren):ps, curr) (Cd name') = if name' == ".." then (ps, Dir pname (curr:pchildren)) else (curr:p:ps, Dir name' [])
acc (ps, (Dir name children)) (FileL name' size) = (ps, (Dir name ((File name' size):children)))
acc x _ = x

part1 :: IO ()
part1 = do
  input <- readFile "../input/day07.txt"
  -- input <- readFile "../input/day07-example.txt"
  -- skip cd / because we manually make that node
  let inp = map parse (drop 1 (lines input))
  let tree = foldl acc ([], Dir "/" []) inp
  -- pop back up to root
  let tree' = snd $ (iterate ((flip acc) (Cd "..")) tree) !! (length (fst tree))
  -- print tree'
  print $ sum $ filter (\s -> s <= 100000) $ map nodeSize $ listDirs tree'

part2 :: IO ()
part2 = do
  input <- readFile "../input/day07.txt"
  let inp = map parse (drop 1 (lines input))
  let tree = foldl acc ([], Dir "/" []) inp
  -- pop back up to root
  let tree' = snd $ (iterate ((flip acc) (Cd "..")) tree) !! (length (fst tree))
  let freeSpace = 70000000 - nodeSize tree'
  let minDelete = 30000000 - freeSpace
  let dirSizes = map nodeSize $ listDirs tree'
  print $ head $ filter ((<=) minDelete) $ sort dirSizes