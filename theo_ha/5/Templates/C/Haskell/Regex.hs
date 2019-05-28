module Regex (Regex(..), empty, epsilon, single, conc, alt, star, isEmpty, isEpsilon, parseRegex, nullable) where

import Control.Arrow (first, second)
import Data.List
import Data.Char (isAlphaNum, isSpace)

data Regex = Empty | Epsilon | Single Char | Conc [Regex] | Alt [Regex] | Star Regex
  deriving (Eq, Ord)

instance Show Regex where
  show = aux 0
    where aux :: Int -> Regex -> String
          aux _    Epsilon    = "()"
          aux _    Empty      = "{}"
          aux _    (Single c) = [c]
          aux prec (Alt rs)   = parens prec 0 (intercalate "|" (map (aux 0) rs))
          aux prec (Conc rs)  = parens prec 1 (concatMap (aux 1) rs)
          aux prec (Star r)   = aux 2 r ++ "*"
          parens a b s = if a > b then "(" ++ s ++ ")" else s


isEmpty :: Regex -> Bool
isEmpty Empty = True
isEmpty _ = False

isEpsilon :: Regex -> Bool
isEpsilon Epsilon = True
isEpsilon _ = False

sortNub :: Ord a => [a] -> [a]
sortNub = sort . nub

size :: Integral a => Regex -> a
size Empty      = 1
size Epsilon    = 1
size (Single _) = 1
size (Alt rs)   = sum (map size rs) + 1
size (Conc rs)  = sum (map size rs) + 1
size (Star r)   = size r + 1

empty :: Regex
empty = Empty

epsilon :: Regex
epsilon = Epsilon

single :: Char -> Regex
single = Single

nullable :: Regex -> Bool
nullable Empty      = False
nullable Epsilon    = True
nullable (Single _) = False
nullable (Conc rs)  = all nullable rs
nullable (Alt rs)   = any nullable rs
nullable (Star r)   = True

alt :: [Regex] -> Regex
alt xs =
  case xs2 of
    []  -> Empty
    [x] -> x
    _   -> Alt xs2
  where check 
          | any (\r -> not (isEpsilon r) && nullable r) xs = 
              (\r -> isEmpty r || isEpsilon r)
          | otherwise = isEmpty
        xs1 = filter (not . check) xs
        xs2 = sortNub (concatMap (\r -> case r of {Alt xs -> xs; r' -> [r']}) xs1)

conc :: [Regex] -> Regex
conc xs | any isEmpty xs = Empty
conc xs = 
  case xs'' of
    []  -> Epsilon
    [x] -> x
    _   -> Conc xs''
  where xs' = filter (not . isEpsilon) xs
        xs'' = concatMap (\r -> case r of {Conc xs -> xs; r' -> [r']}) xs'

star :: Regex -> Regex
star Empty    = Epsilon
star Epsilon  = Epsilon
star (Star r) = Star r
star r        = Star r




-- this is a very ad-hoc parser, but it seems to work well
isSymbol :: Char -> Bool
isSymbol = isAlphaNum

parseRegex :: String -> Regex
parseRegex s = case parse1 (filter (not . isSpace) s) of
                 (rs, []) -> alt rs
                 (_, c : _)  -> error $ "Expected end of input, but found '" ++ [c] ++ "'"
  where term _ [] = True
        term cs (x : _) = x `elem` cs
        
        parse1 s = case s' of
                     '|' : s'' -> first (r :) (parse1 s'')
                     ')' : _   -> ([r], s')
                     []        -> ([r], [])
                     c : _     -> error $ "Unexpected character '" ++ [c] ++ "'" 
          where (r, s') = first conc (parse2 s)
        
        parse2 s | term "|)" s = ([], s)
        parse2 s = if term "|)" s' then ([r], s') else first (r :) (parse2 s')
          where (r, s') = parse3 s
          
        parseStar r ('*' : s) = parseStar (star r) s
        parseStar r s         = (r, s)
        
        parse3 ('{' : '}' : s) = parseStar empty s
        parse3 ('∅' : s) = parseStar empty s
        parse3 ('ε' : s) = parseStar epsilon s
        parse3 ('*' : s) = parseStar epsilon s
        parse3 ('(' : s) = case parse1 s of
                             (rs, ')' : s') -> parseStar (alt rs) s'
                             _              -> error "Missing closing ')'"
        parse3 (c : s) | isSymbol c = parseStar (single c) s
        parse3 (c : _)   = error $ "Unexpected character '" ++ [c] ++ "'"
        

