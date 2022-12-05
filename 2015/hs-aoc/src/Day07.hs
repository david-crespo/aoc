module Day07 (part1, part2) where

import Text.ParserCombinators.Parsec
import qualified Data.Map as M
import Data.Either (fromRight)
import Data.Bits ((.&.), (.|.), shiftR, shiftL, complement)
import Data.Maybe (fromMaybe)
import Data.Word (Word16)
import Data.List (intercalate)

data Node = Wire String | Value Word16
  deriving Show

data Expr = 
  And Node Node
  | Or Node Node
  | RShift Node Int
  | LShift Node Int
  | Not Node
  | Direct Node
  deriving Show

type Program = M.Map String Expr

eand n1 n2 w = (w, And n1 n2)
eor n1 n2 w = (w, Or n1 n2)
ers n1 n2 w = (w, RShift n1 n2)
els n1 n2 w = (w, LShift n1 n2)
enot n w = (w, Not n)
edirect n w = (w, Direct n)

val :: GenParser Char st Word16
val = read <$> many1 digit

int :: GenParser Char st Int
int = read <$> many1 digit

wire :: GenParser Char st String
wire = many1 lower

node :: GenParser Char st Node
node = (Value <$> val) <|> (Wire <$> wire)

expr :: GenParser Char st (String, Expr)
expr = choice [
    eand <$> try (node <* string " AND ") <*> node <*> (string " -> " *> wire),
    eor <$> try (node <* string " OR ") <*> node <*> (string " -> " *> wire),
    ers <$> try (node <* string " RSHIFT ") <*> int <*> (string " -> " *> wire),
    els <$> try (node <* string " LSHIFT ") <*> int <*> (string " -> " *> wire),
    enot <$> (try (string "NOT ") *> node) <*> (string " -> " *> wire),
    edirect <$> node <*> (string " -> " *> wire)
  ]


prog :: GenParser Char st [(String, Expr)]
prog = many (expr <* newline)

dExpr :: Expr
dExpr = Not (Value 0)

eval :: Program -> Node -> Word16
eval p (Wire s) = exec p $ fromMaybe dExpr (M.lookup s p)
eval _ (Value v) = v


exec :: Program -> Expr -> Word16
exec p (And a b) = eval p a .&. eval p b
exec p (Or a b) = eval p a .|. eval p b
exec p (LShift a b) = shiftL (eval p a) b
exec p (RShift a b) = shiftR (eval p a) b
exec p (Not a) = complement $ eval p a
exec p (Direct a) = eval p a

example :: String
example = "123 -> x\n\
\456 -> y\n\
\x AND y -> d\n\
\x OR y -> e\n\
\x LSHIFT 2 -> f\n\
\y RSHIFT 2 -> g\n\
\NOT x -> h\n\
\NOT y -> i\n"

pp :: Show a => [a] -> IO ()
pp xs = putStrLn $ intercalate "\n" $ map show xs

exclude = ["a", "ma", "lz"]

part1 :: IO ()
part1 = do
  input <- readFile "../input/day07.txt"
  -- ugh wish I knew how to do this the proper monad way
  -- let cmds = fromRight [] $ parse prog "(unknown)" input
  let cmds = M.fromList $ fromRight [] $ parse prog "(unknown)" input
  -- let cmds = M.fromList $ fromRight [] $ parse prog "(unknown)" example

  -- by going through the wires one by one (conveniently they get worse in
  -- alphabetical order), I can see that the time each one takes goes up a lot,
  -- to the point where by "ds" it's taking many seconds. so while I could just leave 
  -- it running and see if it ever finishes, it might not every finish. I have to
  -- figure out how to memoize. I didn't realize the same wire gets used in multiple
  -- places, so it's probably exponential complexity without memoization
  let keys = filter (\k -> not (elem k exclude)) $ take 170 $ map fst $ M.toList cmds
  print keys
  pp $ map (\k -> k ++ " " ++ show (eval cmds (Wire k))) keys
  -- print $ eval cmds (Wire "jo")
  -- print $ eval cmds (Wire "ma")
  -- print $ eval cmds (Wire "e")
  -- print $ eval cmds (Wire "f")
  -- print $ eval cmds (Wire "g")
  -- print $ eval cmds (Wire "h")
  -- print $ eval cmds (Wire "i")
  -- print $ eval cmds (Wire "x")
  -- print $ eval cmds (Wire "y")

-- not -1

part2 :: IO ()
part2 = do
  input <- readFile "../input/day07.txt"
  print $ "part2"