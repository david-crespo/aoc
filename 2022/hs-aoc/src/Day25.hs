module Day25 (part1, part2) where

toBase :: Int -> Int -> [Int]
toBase _ 0 = [0]
toBase base n = reverse $ snd $ foldl f (n, []) pows where
  f :: (Int, [Int]) -> Int -> (Int, [Int])
  f (n', acc) pow = (n' - (digit * (base ^ pow)), digit:acc) where
    digit = n' `div` (base ^ pow)

  powmax = floor $ logBase (fromIntegral base) (fromIntegral n)
  pows = reverse [0..powmax]

toSnafu :: Int -> String
toSnafu n = (if ex > 0 then [toChar ex] else []) ++ result where
  (ex, result) = foldr f (0, "") (toBase 5 n)

  -- start from the right end of the base 5 digits and correct them
  -- down, carrying the remainder left as "extra"
  f :: Int -> (Int, String) -> (Int, String)
  f d (extra, acc) = (extra', (toChar cv'):acc) where
    cv = d + extra
    cv' = if cv > 2 then cv - 5 else cv
    extra' = if cv > 2 then 1 else 0

  toChar :: Int -> Char
  toChar 2 = '2'
  toChar 1 = '1'
  toChar (-1) = '-'
  toChar (-2) = '='
  toChar _ = '0'

fromSnafu :: String -> Int
fromSnafu s = sum $ map (\(p, d) -> d * 5^p) pairs where
  pairs = zip (reverse [0..(length s-1)]) (map toNum s)
  toNum :: Char -> Int
  toNum '2' = 2
  toNum '1' = 1
  toNum '-' = -1
  toNum '=' = -2
  toNum _ = 0

part1 :: IO ()
part1 = do
  input <- readFile "../input/day25.txt"
  -- input <- readFile "../input/day25-example.txt"
  let s = sum $ map fromSnafu (lines input)
  print s
  print $ toSnafu s

part2 :: IO ()
part2 = do
  -- input <- readFile "../input/day25.txt"
  -- input <- readFile "../input/day25-example.txt"
  print $ "part2"