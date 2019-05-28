import Control.Monad
import Control.Arrow
import Data.List
import Data.Char (isSpace)

import Regex

reverseRegex :: Regex -> Regex
reverseRegex (Alt rs)  = Alt $ map reverseRegex rs
reverseRegex (Conc rs) = Conc . reverse $ map reverseRegex rs
reverseRegex (Star r)  = star $ reverseRegex r
reverseRegex r         = r

evenOddRegex :: Regex -> (Bool, Bool)
evenOddRegex Empty      = (True, True)
evenOddRegex Epsilon    = (True, False)
evenOddRegex (Single _) = (False, True)
evenOddRegex (Star r)   = (evenRegex r, False)
evenOddRegex (Alt rs)   = 
  foldl' (\(ev1, od1) (ev2, od2) -> (ev1 && ev2, od1 && od2))
    (True, True) (map evenOddRegex rs)
evenOddRegex (Conc rs)  = 
  foldl' (\(ev1, od1) (ev2, od2) -> (ev1 && ev2 || od1 && od2, ev1 && od2 || od1 && ev2))
    (True, True) (map evenOddRegex rs)

evenRegex :: Regex -> Bool
evenRegex = fst . evenOddRegex

oddRegex :: Regex -> Bool
oddRegex  = snd . evenOddRegex

remainderRegex :: Regex -> Char -> Regex
remainderRegex r c = undefined -- TODO



trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace        

main =
  do l <- getLine
     when (l /= "END") (processLine l >> main)
  where processLine l =
          case (trim *** trim) (span (/= ';') l) of
            ([c], ';' : r) -> putStrLn $ show $ remainderRegex (parseRegex r) c
            _        -> error $ "Invalid line in input: " ++ l
  

