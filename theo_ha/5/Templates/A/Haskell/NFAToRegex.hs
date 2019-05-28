import Control.Monad
import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.Map as M
import Data.Map (Map)

import Regex
import NFA


-- IMPLEMENTATION

type State = String

nfaToRegex :: Bool -> NFA Char State -> IO Regex
nfaToRegex fullMode nfa
  | not fullMode = return . computeResult $ foldl next init qs
  | otherwise = -- PRINTING CODE - IGNORE THIS
      do putStrLn "BEGIN NFA TO REGEX TRACE"
         printStep 0 Nothing init
         (_, m) <- foldM (\(i, m) q -> let m' = next m q in printStep i (Just q) m' >> return (i+1, m')) (1, init) qs
         let r = computeResult m
         putStrLn $ "Final result: " ++ show r ++ "\nEND NFA TO REGEX TRACE"
         return r

  where lookup m q1 q2 = M.findWithDefault empty (q1, q2) m
        printStep i q m =
          putStrLn $ "Step " ++ show i ++ "\n" ++ maybe "" (\q -> "Processing state " ++ q ++ "\n") q ++
                         unlines ["(" ++ qi ++ ", " ++ qj ++ "): " ++ show r | ((qi, qj), r) <- M.toList m]
        -- END PRINTING CODE

        -- Initialisation: Compute the table α_{ij}^0
        init :: Map (State, State) Regex
        init = undefined -- TODO
        
        -- Perform one step of the construction, i.e. given the table α_{ij}^k and the state q_k,
        -- compute the map α_{ij}^{k+1}
        next :: Map (State, State) Regex -> State -> Map (State, State) Regex
        next m qk = undefined -- TODO

        -- Read off the result from the final table, i.e. given the table α_{ij}^n, return a regular
        -- expression that is equivalent to the entire NFA
        computeResult :: Map (State, State) Regex -> Regex
        computeResult m = undefined -- TODO

-- END IMPLEMENTATION


main =
  do modeStr <- getLine
     if modeStr `elem` ["Simple", "Full"] then do
       nfa <- fmap parseNFA readLines
       r   <- nfaToRegex (modeStr == "Full") nfa
       when (modeStr == "Simple") (putStrLn $ show r)
     else
       error "Invalid mode"
  where readLines = do {s <- getLine; if s == "END" then return [] else fmap (s:) readLines}
  

