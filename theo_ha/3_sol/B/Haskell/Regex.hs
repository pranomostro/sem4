module Regex (Regex(..), parseRegex, getChars) where

import Control.Arrow (first)
import Control.Monad
import Data.Char (isAlphaNum, isSpace)
import qualified Data.Set as S
import Data.Set (Set)

-- Definition of regexes

infixl 6 :|:
infixl 7 :.:

data Regex = Empty | Epsilon | Single Char | Regex :|: Regex | Regex :.: Regex | Star Regex
  deriving (Ord, Eq)

getChars :: Regex -> Set Char
getChars Empty      = S.empty
getChars Epsilon    = S.empty
getChars (Single c) = S.singleton c
getChars (r :.: s)  = S.union (getChars r) (getChars s)
getChars (r :|: s)  = S.union (getChars r) (getChars s)
getChars (Star r)   = getChars r


-- Pretty printing

instance Show Regex where
  show = showRegexPrec 0
    where parens a b s = if a > b then "(" ++ s ++ ")" else s
          showRegexPrec :: Int -> Regex -> String
          showRegexPrec _ Empty      = "∅"
          showRegexPrec _ Epsilon    = "ε"
          showRegexPrec _ (Single c) = [c]
          showRegexPrec p (r :|: s)  = parens p 0 (showRegexPrec 0 r ++ "|" ++ showRegexPrec 0 s)
          showRegexPrec p (r :.: s)  = parens p 1 (showRegexPrec 1 r ++ showRegexPrec 1 s)
          showRegexPrec _ (Star r)   = showRegexPrec 2 r ++ "*"


-- Parsing

isSymbol :: Char -> Bool
isSymbol = isAlphaNum

-- this is a very ad-hoc parser, but it seems to work well
parseRegex :: String -> Regex
parseRegex s = case parse1 (filter (not . isSpace) s) of
                 (r, []) -> r
                 (_, c : _)  -> error $ "Expected end of input, but found '" ++ [c] ++ "'"
  where term _ [] = True
        term cs (x : _) = x `elem` cs
        
        parse1 s = case s' of
                     '|' : s'' -> first (r :|:) (parse1 s'')
                     ')' : _   -> (r, s')
                     []        -> (r, [])
                     c : _     -> error $ "Unexpected character '" ++ [c] ++ "'" 
          where (r, s') = parse2 s
        
        parse2 s | term "|)" s = (Epsilon, s)
        parse2 s = if term "|)" s' then (r, s') else first (r :.:) (parse2 s')
          where (r, s') = parse3 s
          
        parseStar r ('*' : s) = parseStar (Star r) s
        parseStar r s         = (r, s)
        
        parse3 ('{' : '}' : s) = parseStar Empty s
        parse3 ('∅' : s) = parseStar Empty s
        parse3 ('ε' : s) = parseStar Epsilon s
        parse3 ('*' : s) = parseStar (Star Epsilon) s
        parse3 ('(' : s) = case parse1 s of
                             (r, ')' : s') -> parseStar r s'
                             _            -> error "Missing closing ')'"
        parse3 (c : s) | isSymbol c = parseStar (Single c) s
        parse3 (c : _)   = error $ "Unexpected character '" ++ [c] ++ "'"

