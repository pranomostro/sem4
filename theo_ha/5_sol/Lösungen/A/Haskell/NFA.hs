module NFA (NFA(..), printNFA, parseNFA) where

import Data.List
import qualified Data.Set as S
import Data.Set (Set)
import Data.Char

data NFA c s = NFA {states :: Set s, alphabet :: Set c, 
                    transitions :: Set (s, c, s), startState :: s, finalStates :: Set s}
  deriving Show

-- Printing

printNFA :: (s -> String) -> NFA Char s -> IO ()
printNFA showState m =
  do putStrLn "NFA"
     putStrLn $ "Alphabet: " ++ intersperse ';' (S.toList (alphabet m))
     putStrLn $ "States: " ++ intercalate ";" (map showState (S.toList (states m)))
     putStrLn $ "Init: " ++ showState (startState m)
     putStrLn $ "Final: " ++ intercalate ";" (map showState (S.toList (finalStates m)))
     putStrLn "Transitions:"
     mapM_ (putStrLn . showTransition) (S.toList (transitions m))
     putStrLn "END"
  where showTransition (q, c, q') = showState q ++ ";" ++ [c] ++ ";" ++ showState q'

parseNFA :: [String] -> NFA Char String
parseNFA (nfa : alph : st : init : fin : tr : trs)
  | trim nfa /= "NFA" = error $ "Expected token 'NFA', but got '" ++ trim nfa ++ "'"
  | trim tr /= "Transitions:" = error $ "Expected token 'Transitions', but got '" ++ trim tr ++ "'"
  | otherwise = NFA {states = parseStates st, alphabet = parseAlphabet alph, startState = parseInit init,
                     finalStates = parseFinal fin, transitions = S.fromList (map parseTransition trs)}
  where trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
        stripLabel s1 s2 =
          case stripPrefix (s1 ++ ":") s2 of
            Nothing -> error ("Expected entry '" ++ s1 ++ "', but got '" ++ s2 ++ "'")
            Just s  -> trim s

        splitOn :: Eq a => a -> [a] -> [[a]]
        splitOn _ [] = []
        splitOn c s  = case span (/= c) s of
                         (s1, []) -> [s1]
                         (s1, _ : s2) -> s1 : splitOn c s2

        parseChar [c] = c
        parseChar s   = error $ "Expected single character, but got '" ++ s ++ "'"
        parseAlphabet = S.fromList . map (parseChar . trim) . splitOn ';' . stripLabel "Alphabet"
        parseStates = S.fromList . map trim . splitOn ';' . stripLabel "States"
        parseInit = trim . stripLabel "Init"
        parseFinal = S.fromList . map trim . splitOn ';' . stripLabel "Final"
        parseTransition s =
          case map trim (splitOn ';' s) of
            [a, b, c] -> (a, parseChar b, c)
            _         -> error $ "Invalid transition: '" ++ s ++ "'"                    
parseNFA _ = error "Invalid NFA format"

