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
remainderRegex Empty       _ = empty
remainderRegex Epsilon     _ = empty
remainderRegex (Single c') c = if c == c' then epsilon else empty
remainderRegex (Star r)    c = conc [star r, remainderRegex r c]
remainderRegex (Alt rs)    c = alt [remainderRegex r c | r <- rs]
remainderRegex (Conc rs)   c = alt $ go (reverse rs)
  where go [] = []
        go (r : rs) = conc (reverse rs ++ [remainderRegex r c]) : (if nullable r then go rs else [])



trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace        

main =
  do l <- getLine
     when (l /= "END") (processLine l >> main)
  where processLine l =
          case (trim *** trim) (span (/= ';') l) of
            ([c], ';' : r) -> putStrLn $ show $ remainderRegex (parseRegex r) c
            _        -> error $ "Invalid line in input: " ++ l
  

