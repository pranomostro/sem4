module Main (main) where

import Control.Monad
import Data.Char

data Symbol = Terminal Char | Nonterminal Char
  deriving (Eq, Ord)

data Production = [Symbol] :->: [Symbol]
  deriving (Eq, Ord)

startSymbol :: Symbol
startSymbol = Nonterminal 'S'


-----------------------------------------------------------------------------------------------------------------------
-- Pretty printing
-----------------------------------------------------------------------------------------------------------------------

instance Show Symbol where
  show (Terminal c) = [c]
  show (Nonterminal c) = [c]

showSymbols :: [Symbol] -> String
showSymbols [] = "ε"
showSymbols xs = concatMap show xs

instance Show Production where
  show (lhs :->: rhs) = showSymbols lhs ++ " → " ++ showSymbols rhs


-----------------------------------------------------------------------------------------------------------------------
-- Parsing
-----------------------------------------------------------------------------------------------------------------------

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

parseSymbol :: Char -> Symbol
parseSymbol c
  | isUpper c = Nonterminal c
  | isLower c = Terminal c
  | otherwise = error ("Invalid symbol: '" ++ [c] ++ "'")

parseProduction :: String -> Production
parseProduction s =
  case span (\c -> c /= '-' && c /= '→') s of
    (lhs, '-' : '>' : rhs) -> go lhs rhs
    (lhs, '→' : rhs) -> go lhs rhs
  where go lhs rhs = map parseSymbol (trim lhs) :->: map parseSymbol (trim rhs)


-----------------------------------------------------------------------------------------------------------------------
-- Implementation
-----------------------------------------------------------------------------------------------------------------------

getType :: Production -> Int
getType p | p == [startSymbol] :->: [] = 3
getType (lhs :->: rhs) | length lhs > length rhs = 0
getType ([Nonterminal _] :->: rhs) =
  case rhs of
    [Terminal _] -> 3
    [Terminal _, Nonterminal _] -> 3
    _ -> 2
getType _ = 1

main =
  do s <- getLine
     when (s /= "END") $ print (getType (parseProduction s)) >> main


