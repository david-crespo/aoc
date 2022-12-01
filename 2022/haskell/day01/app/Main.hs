import Data.List (groupBy, sort)

readInt :: String -> Int
readInt = read

groupfn :: [[String]] -> String -> [[String]]
groupfn [] y = [[y]]
groupfn (x : xs) y = if y == "" then [] : x : xs else (y : x) : xs

sums :: String -> [Int]
sums input = map (sum . map readInt) $ foldl groupfn [] (lines input)

part1 :: IO ()
part1 = do
  input <- readFile "../../input/day01.txt"
  print $ foldl max 0 (sums input)

part2 :: IO ()
part2 = do
  input <- readFile "../../input/day01.txt"
  print $ sum $ take 3 $ reverse $ sort (sums input)

main = do
  part1
  part2