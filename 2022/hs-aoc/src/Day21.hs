module Day21 (part1, part2) where

import Text.ParserCombinators.Parsec
import qualified Data.Map as M
import Data.Either (fromRight)

data Expr = 
  Mult String String
  | Add String String
  | Div String String
  | Sub String String
  | Eq String String
  | Direct Int
  | Result Bool
  deriving (Show, Eq)

type Program = M.Map String Expr

int :: GenParser Char st Int
int = read <$> many1 digit

monkey :: GenParser Char st String
monkey = many1 lower

line :: GenParser Char st (String, Expr)
line = (,) <$> (monkey <* string ": ") <*> expr

root :: GenParser Char st (String, Expr)
root = (,) <$> (string "root" <* string ": ") <*> rootExpr

rootExpr :: GenParser Char st Expr
rootExpr = (Eq <$> (monkey <* string " + ")) <*> monkey

expr :: GenParser Char st Expr
expr = choice [
    Mult <$> try (monkey <* string " * ") <*> monkey,
    Add <$> try (monkey <* string " + ") <*> monkey,
    Div <$> try (monkey <* string " / ") <*> monkey,
    Sub <$> try (monkey <* string " - ") <*> monkey,
    Direct <$> int
  ]

prog :: GenParser Char st [(String, Expr)]
prog = many (line <* newline)

prog2 :: GenParser Char st [(String, Expr)]
prog2 = many ((try root <|> line) <* newline)

-- A program is done if all its expressions are resolved to number values
done :: Program -> Bool
done p = all (doneExpr . snd) (M.toList p) where
  doneExpr :: Expr -> Bool
  doneExpr (Direct _) = True
  doneExpr _ = False

-- go through and resolve the resolveable expressions
step :: Program -> Program
step p = M.map tryReplace p where
  tryReplace :: Expr -> Expr
  tryReplace x@(Mult a b) = maybe x Direct (tryReplace' (*) a b)
  tryReplace x@(Add a b) = maybe x Direct (tryReplace' (+) a b)
  tryReplace x@(Div a b) = maybe x Direct (tryReplace' div a b)
  tryReplace x@(Sub a b) = maybe x Direct (tryReplace' (-) a b)
  tryReplace x@(Eq a b) = maybe x Direct (tryReplace' (-) a b)
  tryReplace x = x

  tryReplace' ::  (Int -> Int -> Int) -> String -> String -> Maybe Int
  tryReplace' op a b = case (M.lookup a p, M.lookup b p) of (Just (Direct v1), Just (Direct v2)) -> Just (op v1 v2)
                                                            _ -> Nothing

part1 :: IO ()
part1 = do
  input <- readFile "../input/day21.txt"
  -- input <- readFile "../input/day21-example.txt"
  let cmds = M.fromList $ fromRight [] $ parse prog "(unknown)" input
  print $ M.lookup "root" $ until done step cmds

expSearch :: (Int -> Int) -> [Int] -> [Int]
expSearch f (n:ns) = if v == 0 then (n:ns)
                     -- for example input this > must be switched to <
                     else if v > 0 then expSearch f (nextUp:n:ns) 
                     else expSearch f (nextDown:(n:ns)) where
  v = f n

  m = if length ms > 0 then minimum ms else 0 where
    ms = filter (> n) ns
  nextUp = if n > m then n * 10 else ((n + m) `div` 2)

  m' = if length ms > 0 then maximum ms else 0 where
    ms = filter (< n) ns
  nextDown = (n + m') `div` 2

part2 :: IO ()
part2 = do
  input <- readFile "../input/day21.txt"
  -- input <- readFile "../input/day21-example.txt"
  let cmds = M.delete "humn" $ M.fromList $ fromRight [] $ parse prog2 "(unknown)" input
  -- initially solved with a manual exponential search because I'm an idiot
  -- print $ [(x, M.lookup "root" $ until done step (M.insert "humn" (Direct x) cmds)) | x <- [3093175982595]]
  let run x = (\(Just (Direct result)) -> result) $ M.lookup "root" $ until done step (M.insert "humn" (Direct x) cmds)
  print $ expSearch run [1]