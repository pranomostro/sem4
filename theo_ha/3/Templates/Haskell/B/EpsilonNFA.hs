module EpsilonNFA (EpsilonNFA(..), printEpsilonNFA) where

import Data.Char (isSpace)
import Data.List
import qualified Data.Set as S
import Data.Set (Set)

data EpsilonNFA c s = EpsilonNFA {states :: Set s, alphabet :: Set c, 
                                  transitions :: Set (s, Maybe c, s), startState :: s, finalStates :: Set s}
  deriving Show

-- Printing

printEpsilonNFA :: (s -> String) -> EpsilonNFA Char s -> IO ()
printEpsilonNFA showState m =
  do putStrLn "EpsilonNFA"
     putStrLn $ "Alphabet: " ++ intersperse ';' (S.toList (alphabet m))
     putStrLn $ "States: " ++ intercalate ";" (map showState (S.toList (states m)))
     putStrLn $ "Init: " ++ showState (startState m)
     putStrLn $ "Final: " ++ intercalate ";" (map showState (S.toList (finalStates m)))
     putStrLn "Transitions:"
     mapM_ (putStrLn . showTransition) (S.toList (transitions m))
     putStrLn "END"
  where showTransition (q, c, q') = showState q ++ ";" ++ maybe "" return c ++ ";" ++ showState q'


