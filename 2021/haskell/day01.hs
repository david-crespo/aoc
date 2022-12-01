readInt :: String -> Int
readInt = read

part1 :: IO ()
part1 = do
  input <- readFile "input/day01.txt"
  let nums = map readInt (lines input)
  let pairs = zip nums (tail nums)
  print $ length $ filter (uncurry (<)) pairs

sum3 (a, b, c) = a + b + c

part2 :: IO ()
part2 = do
  input <- readFile "input/day01.txt"
  let nums = map readInt (lines input)
  let sums = map sum3 $ zip3 nums (tail nums) (tail (tail nums))
  let pairs = zip sums (tail sums)
  print $ length $ filter (uncurry (<)) pairs

main = part2