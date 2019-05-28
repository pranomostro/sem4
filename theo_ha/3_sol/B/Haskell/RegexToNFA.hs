import Control.Applicative
import Control.Monad
import qualified Data.Set as S
import Data.Set (Set)

import System.IO
import qualified Data.ByteString as BS
import Data.Word
import Data.Bits
import Data.Char (chr, ord, isSpace)

import Regex
import EpsilonNFA


-- "Fresh"-Monade, um frische Zustandsnamen zu generieren.
-- Können Sie benutzen. Müssen Sie aber auch nicht.
newtype Fresh a = Fresh {runFresh :: Int -> (a, Int)}

instance Functor Fresh where
  fmap = liftM

instance Applicative Fresh where
  pure = return
  (<*>) = ap

instance Monad Fresh where
  return x = Fresh (\s -> (x, s))
  Fresh f >>= g = Fresh (\s -> case f s of (x, s') -> runFresh (g x) s')

fresh :: Fresh Int
fresh = Fresh (\s -> (s, s + 1))

-- Berechnet einen NFA für den gegebenen Regex mit dem gegebenen Alphabet
regexToEpsilonNFA :: Set Char -> Regex -> EpsilonNFA Char Int
regexToEpsilonNFA alph r = fst (runFresh (go r) 0)
  where newState = fresh

        go Empty =
          do q <- newState
             return $ EpsilonNFA {states = S.singleton q, alphabet = alph, startState = q, finalStates = S.empty, transitions = S.empty}

        go Epsilon =
          do q <- newState
             return $ EpsilonNFA {states = S.singleton q, alphabet = alph, startState = q, finalStates = S.singleton q, transitions = S.empty}

        go (Single c) =
          do q1 <- newState
             q2 <- newState
             return $ EpsilonNFA {states = S.fromList [q1, q2], alphabet = alph, startState = q1, finalStates = S.singleton q2,
                           transitions = S.singleton (q1, Just c, q2)}

        go (r :.: s) =
          do m1 <- go r
             m2 <- go s
             let newTransitions = [(q, Nothing, startState m2) | q <- S.toList (finalStates m1)]
             return $ EpsilonNFA {alphabet = alph, startState = startState m1, finalStates = finalStates m2,
                                  states = S.union (states m1) (states m2),
                                  transitions = S.unions [S.fromList newTransitions, transitions m1, transitions m2]}

        go (r :|: s) =
          do q <- newState
             m1 <- go r
             m2 <- go s
             let newTransitions = S.fromList [(q, Nothing, startState m) | m <- [m1, m2]]
             return $ EpsilonNFA {alphabet = alph, startState = q, finalStates = S.union (finalStates m1) (finalStates m2),
                                  states = S.insert q (S.union (states m1) (states m2)),
                                  transitions = S.unions [newTransitions, transitions m1, transitions m2]}

        go (Star r) =
          do q <- newState
             m <- go r
             let newTransitions = (q, Nothing, startState m) : [(q', Nothing, startState m) | q' <- S.toList (finalStates m)]
             return $ EpsilonNFA {alphabet = alph, startState = q, finalStates = S.insert q (finalStates m),
                                  states = S.insert q (states m),
                                  transitions = S.union (S.fromList newTransitions) (transitions m)}


-- Eingabe/Ausgabe

main =
  do alphabet <- fmap (S.fromList . filter (\c -> not (isSpace c) && c /= ';')) myGetLine
     s <- myGetLine
     printEpsilonNFA show (regexToEpsilonNFA alphabet (parseRegex s))


-- Some UTF8 magic required because the TUMjudge uses a strange locale

myGetLine = fmap decodeUtf8 BS.getLine
myPrintLine s = BS.putStr . encodeUtf8 $ s ++ "\n"

encodeUtf8 :: String -> BS.ByteString
encodeUtf8 = BS.pack . encode

decodeUtf8 :: BS.ByteString -> String
decodeUtf8 = decode . BS.unpack

replacementCharacter :: Char
replacementCharacter = '\xfffd'

encode :: String -> [Word8]
encode = concatMap (map fromIntegral . go . ord)
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `shiftR` 6)
                        , 0x80 + oc .&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `shiftR` 12)
                        , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                        , 0x80 + oc .&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `shiftR` 18)
                        , 0x80 + ((oc `shiftR` 12) .&. 0x3f)
                        , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                        , 0x80 + oc .&. 0x3f
                        ]

decode :: [Word8] -> String
decode [    ] = ""
decode (c:cs)
  | c < 0x80  = chr (fromEnum c) : decode cs
  | c < 0xc0  = replacementCharacter : decode cs
  | c < 0xe0  = multi1
  | c < 0xf0  = multi_byte 2 0xf  0x800
  | c < 0xf8  = multi_byte 3 0x7  0x10000
  | c < 0xfc  = multi_byte 4 0x3  0x200000
  | c < 0xfe  = multi_byte 5 0x1  0x4000000
  | otherwise = replacementCharacter : decode cs
  where
    multi1 = case cs of
      c1 : ds | c1 .&. 0xc0 == 0x80 ->
        let d = ((fromEnum c .&. 0x1f) `shiftL` 6) .|.  fromEnum (c1 .&. 0x3f)
        in if d >= 0x000080 then toEnum d : decode ds
                            else replacementCharacter : decode ds
      _ -> replacementCharacter : decode cs

    multi_byte :: Int -> Word8 -> Int -> String
    multi_byte i mask overlong = aux i cs (fromEnum (c .&. mask))
      where
        aux 0 rs acc
          | overlong <= acc && acc <= 0x10ffff &&
            (acc < 0xd800 || 0xdfff < acc)     &&
            (acc < 0xfffe || 0xffff < acc)      = chr acc : decode rs
          | otherwise = replacementCharacter : decode rs

        aux n (r:rs) acc
          | r .&. 0xc0 == 0x80 = aux (n-1) rs
                               $ shiftL acc 6 .|. fromEnum (r .&. 0x3f)

        aux _ rs     _ = replacementCharacter : decode rs

