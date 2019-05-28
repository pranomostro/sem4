module DFA (DFA(..), printDFA) where

import Data.Char (isSpace)
import Data.List
import qualified Data.Set as S
import Data.Set (Set)

data DFA c s = DFA {states :: Set s, alphabet :: Set c, 
                    transitions :: Set (s, c, s), startState :: s, finalStates :: Set s}
  deriving Show

-- Printing

printDFA :: (s -> String) -> DFA Char s -> IO ()
printDFA showState m =
  do putStrLn "DFA"
     putStrLn $ "Alphabet: " ++ intersperse ';' (S.toList (alphabet m))
     putStrLn $ "States: " ++ intercalate ";" (map showState (S.toList (states m)))
     putStrLn $ "Init: " ++ showState (startState m)
     putStrLn $ "Final: " ++ intercalate ";" (map showState (S.toList (finalStates m)))
     putStrLn "Transitions:"
     mapM_ (putStrLn . showTransition) (S.toList (transitions m))
     putStrLn "END"
  where showTransition (q, c, q') = showState q ++ ";" ++ [c] ++ ";" ++ showState q'


