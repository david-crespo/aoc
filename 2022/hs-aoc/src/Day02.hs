module Day02
  ( part1,
    part2,
  )
where

score :: String -> Int
score "A X" = 1 + 3
score "A Y" = 2 + 6
score "A Z" = 3
score "B X" = 1 
score "B Y" = 2 + 3
score "B Z" = 3 + 6
score "C X" = 1 + 6
score "C Y" = 2 
score "C Z" = 3 + 3 
score _  = 0

score2 :: String -> Int
score2 "A X" = 3 
score2 "A Y" = 1 + 3
score2 "A Z" = 2 + 6
score2 "B X" = 1 
score2 "B Y" = 2 + 3
score2 "B Z" = 3 + 6
score2 "C X" = 2 
score2 "C Y" = 3 + 3
score2 "C Z" = 1 + 6 
score2 _  = 0

part1 :: IO ()
part1 = do
  input <- readFile "../input/day02.txt"
  print $ sum $ map score (lines input)

part2 :: IO ()
part2 = do
  input <- readFile "../input/day02.txt"
  print $ sum $ map score2 (lines input)