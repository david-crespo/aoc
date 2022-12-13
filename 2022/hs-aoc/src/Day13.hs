module Day13 (part1, part2) where

import Text.ParserCombinators.Parsec
import Data.Either (fromRight)
import Data.List (findIndices, sort)

data Packet = L [Packet] | I Int deriving (Show, Eq)

instance Ord Packet where
  compare (L (a:as)) (L (b:bs)) = case compare a b of EQ -> compare (L as) (L bs)
                                                      o -> o
  compare (L []) (L (_:_)) = LT
  compare (L (_:_)) (L []) = GT
  compare (L []) (L []) = EQ
  compare (I a) (L bs) = compare (L [I a]) (L bs)
  compare (L as) (I b) = compare (L as) (L [I b])
  compare (I a) (I b) = compare a b

num :: GenParser Char st Packet
num = I . read <$> many1 digit

lst :: GenParser Char st Packet
lst = L <$> between (string "[") (string "]") (sepBy packet (string ","))

packet :: GenParser Char st Packet
packet = lst <|> num

pair :: GenParser Char st (Packet, Packet)
pair = (,) <$> (packet <* newline) <*> packet

part1 :: IO ()
part1 = do
  input <- readFile "../input/day13.txt"
  -- input <- readFile "../input/day13-example.txt"
  let pairs = fromRight [] $ parse (sepBy pair (string "\n\n")) "(unknown)" input
  print $ sum $ map fst $ filter ((== LT) . snd) $ zip [1..] (map (uncurry compare) pairs)

part2 :: IO ()
part2 = do
  input <- readFile "../input/day13.txt"
  -- input <- readFile "../input/day13-example.txt"
  let pairs = fromRight [] $ parse (sepBy pair (string "\n\n")) "(unknown)" input
  let markers = [L [L [I 2]], L [L [I 6]]]
  let packets = concatMap (\(a,b) -> [a,b]) pairs ++ markers
  print $ product $ map (+1) $ findIndices (`elem` markers) $ sort packets