import Control.Arrow
import Control.Monad
import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe
import Data.List
import Data.Char hiding (isSymbol)
import Control.Arrow (first)
import qualified Data.ByteString as BS
import Data.Word
import Data.Bits

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

remainderRegex' :: Regex -> String -> Regex
remainderRegex' = foldr (flip remainderRegex)

matches :: Regex -> String -> Bool
matches r w = nullable (remainderRegex' r w)

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

trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace        

split :: String -> [String]
split s = case span (/= ';') s of
            (s', []) -> [s']
            (s', _ : s'') -> s' : split s''

main =
  do l <- myGetLine
     when (l /= "END") (processLine (map trim (split l)) >> main)
  where processLine (r : xs) =
          let r' = parseRegex r
          in  myPutStrLn $ intercalate "; " [x ++ ": " ++ (if matches r' x then "y" else "n") | x <- xs]
  


myGetLine = fmap decodeUtf8 BS.getLine
myPutStrLn s = BS.putStr . encodeUtf8 $ s ++ "\n"

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


