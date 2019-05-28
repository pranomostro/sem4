module Main (main) where

import Control.Arrow
import Control.Monad
import Data.Char
import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.Map as M
import Data.Map (Map)
import Data.List
import Data.Maybe
import Data.Ord
import System.IO

data Symbol = Terminal Char | Nonterminal Char
  deriving (Eq, Ord)

data Production = [Symbol] :->: [Symbol]
  deriving (Eq, Ord)
  
data CSG = CSG {startSymbol :: Char, productions :: Set Production}
  deriving (Ord, Eq)

startWord :: CSG -> [Symbol]
startWord g = [Nonterminal (startSymbol g)]

trivialProduction :: CSG -> Production
trivialProduction g = startWord g :->: []


-----------------------------------------------------------------------------------------------------------------------
-- Pretty printing
-----------------------------------------------------------------------------------------------------------------------

instance Show Symbol where
  show (Terminal c) = [c]
  show (Nonterminal c) = [c]

showWord :: [Symbol] -> String
showWord xs = "\"" ++ concatMap show xs ++ "\""

showWord' :: String -> String
showWord' xs = "\"" ++ xs ++ "\""

instance Show Production where
  show (lhs :->: rhs) = concatMap show lhs ++ " -> " ++ concatMap show rhs


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

isValidProduction :: Bool -> Char -> Production -> Bool
isValidProduction hasEps s p@(lhs :->: rhs)
  | p == [Nonterminal s] :->: [] = True
  | hasEps && Nonterminal s `elem` rhs = False
  | otherwise = length lhs <= length rhs

parseProduction :: String -> Production
parseProduction s =
  case span (\c -> c /= '-' && c /= '→') s of
    (lhs, '-' : '>' : rhs) -> go lhs rhs
    (lhs, '→' : rhs) -> go lhs rhs
    _ -> error $ "Failed to parse production: '" ++ s ++ "'"
  where go lhs rhs = map parseSymbol (trim lhs) :->: map parseSymbol (trim rhs)

csg :: Char -> Set Production -> CSG
csg s ps
  | not (isUpper s) = error $ "Illegal start symbol: '" ++ [s] ++ "'"
  | not (all valid (S.toList ps)) =
      error $ "Illegal production: '" ++ show (head (filter (not . valid) (S.toList ps)))
  | otherwise = CSG s ps
  where hasEps = ([Nonterminal s] :->: []) `S.member` ps
        valid = isValidProduction hasEps s


parseCSG :: [String] -> CSG
parseCSG ("CSG" : startLine : "Productions:" : ps) =
  case fmap trim (stripPrefix "Start symbol:" startLine) of
    Just [s] -> csg s (S.fromList $ map parseProduction ps)
    _ -> error $ "Expected 'Start symbol' but got '" ++ startLine ++ "' instead."
parseCSG _ = error "Could not parse CSG definition"

instance Show CSG where
  show (CSG s ps) = "CSG\nStart symbol: " ++ [s] ++ "\nProductions:\n" ++ intercalate "\n" (map show (S.toList ps)) ++ "\nEND\n"


-----------------------------------------------------------------------------------------------------------------------
-- Implementation
-----------------------------------------------------------------------------------------------------------------------

apply :: Production -> [Symbol] -> [[Symbol]]
apply p w = undefined -- TODO
    
enumerateWordsRaw :: CSG -> Int -> Set [Symbol]
enumerateWordsRaw g l = undefined -- TODO
        
enumerateWords :: CSG -> Int -> Set String
enumerateWords g l = undefined -- TODO
        
        
        
-----------------------------------------------------------------------------------------------------------------------
-- Input / output
-----------------------------------------------------------------------------------------------------------------------

main =
  do mode <- getLine
     let f g l = if mode == "Raw" then map (concatMap show) $ S.toList (enumerateWordsRaw g l)
                                  else S.toList (enumerateWords g l)
     loop f
  where readLines = do {s <- getLine; if s == "END" then return [] else fmap (s:) readLines}
        loop f =
          do b <- hIsEOF stdin
             when (not b) $ do g <- fmap (parseCSG . map trim) readLines
                               l <- fmap read getLine
                               mapM_ (putStrLn . showWord') $ sortBy (comparing (\s -> (length s, s))) $ f g l
                               putStrLn "END"
                               loop f



