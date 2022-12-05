module Day07 (part1, part2) where

import Text.ParserCombinators.Parsec
import qualified Data.Map as M
import Data.Either (fromRight)
import Data.Bits ((.&.), (.|.), shiftR, shiftL, complement)
import Data.Word (Word16)
-- import Data.List (intercalate)

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
    binop And <$> try (node <* string " AND ") <*> node <*> (string " -> " *> wire),
    binop Or <$> try (node <* string " OR ") <*> node <*> (string " -> " *> wire),
    binop RShift <$> try (node <* string " RSHIFT ") <*> int <*> (string " -> " *> wire),
    binop LShift <$> try (node <* string " LSHIFT ") <*> int <*> (string " -> " *> wire),
    monop Not <$> (try (string "NOT ") *> node) <*> (string " -> " *> wire),
    monop Direct <$> node <*> (string " -> " *> wire)
  ] where
    binop op n1 n2 w = (w, op n1 n2)
    monop op n w = (w, op n)


prog :: GenParser Char st [(String, Expr)]
prog = many (expr <* newline)

-- example :: String
-- example = "123 -> x\n\
-- \456 -> y\n\
-- \x AND y -> d\n\
-- \x OR y -> e\n\
-- \x LSHIFT 2 -> f\n\
-- \y RSHIFT 2 -> g\n\
-- \NOT x -> h\n\
-- \NOT y -> i\n"

-- A program is done if all its expressions are resolved to number values
done :: Program -> Bool
done p = all (doneExpr . snd) (M.toList p) where
  doneExpr :: Expr -> Bool
  doneExpr (Direct (Value _)) = True
  doneExpr _ = False

mapExpr :: (Node -> Node) -> Expr -> Expr
mapExpr f (And n1 n2) = And (f n1) (f n2)
mapExpr f (Or n1 n2) = Or (f n1) (f n2)
mapExpr f (RShift n i) = RShift (f n) i
mapExpr f (LShift n i) = LShift (f n) i
mapExpr f (Not n) = Not (f n)
mapExpr f (Direct n) = Direct (f n)

-- go through and resolve the resolveable expressions
step :: Program -> Program
step p = M.map stepValue p where
  -- if we look up the key and find a direct value, replace the key with the value
  tryReplace :: Node -> Node
  tryReplace (Wire a) = case M.lookup a p of (Just (Direct (Value v))) -> (Value v)
                                             _ -> (Wire a)
  tryReplace x = x

  -- fully resolve expressions where we have all the values
  stepValue (And (Value a) (Value b)) = Direct (Value (a .&. b))
  stepValue (Or (Value a) (Value b)) = Direct (Value (a .|. b))
  stepValue (LShift (Value a) b) = Direct (Value (shiftL a b))
  stepValue (RShift (Value a) b) = Direct (Value (shiftR a b))
  stepValue (Not (Value a)) = Direct (Value (complement a))

  -- convert wires to their values if possible
  stepValue e = mapExpr tryReplace e

-- pp :: Show a => [a] -> IO ()
-- pp xs = putStrLn $ intercalate "\n" $ map show xs

-- exclude = ["a", "ma", "lz"]

part1 :: IO ()
part1 = do
  input <- readFile "../input/day07.txt"
  let cmds = M.fromList $ fromRight [] $ parse prog "(unknown)" input
  print $ M.lookup "a" $ until done step cmds

  -- let cmds = M.fromList $ fromRight [] $ parse prog "(unknown)" example
  -- pp $ M.toList cmds
  -- print ""

  -- it takes exactly 431 iterations to get there
  -- print $ M.lookup "a" $ iterate step cmds !! 431
  -- pp $ M.toList $ until done step cmds

part2 :: IO ()
part2 = do
  input <- readFile "../input/day07.txt"
  let cmds = M.fromList $ fromRight [] $ parse prog "(unknown)" input
  let newCmds = (M.insert "b" (Direct (Value 16076)) cmds)
  print $ M.lookup "a" $ until done step newCmds