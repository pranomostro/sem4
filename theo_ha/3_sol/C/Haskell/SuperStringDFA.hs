import Data.List
import qualified Data.Set as S
import Data.Set (Set)

import System.IO
import qualified Data.ByteString as BS
import Data.Word
import Data.Bits
import Data.Char (chr, ord)

import DFA

superStringDFA :: Set Char -> String -> DFA Char String
superStringDFA alph s =
  DFA {states = S.fromList (inits s), alphabet = alph, startState = "", finalStates = S.singleton s,
       transitions = S.fromList [(prefix, c, next prefix c) | prefix <- inits s, c <- S.toList alph]}
  where next prefix c = if prefix == s then s else head (filter (`isPrefixOf` s) (tails (prefix ++ [c])))

superStringDFA' :: String -> DFA Char String
superStringDFA' s = superStringDFA (S.fromList s) s

main =
  do s <- getLine
     printDFA showState (superStringDFA' s)
  where showState s  = "_" ++ s


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

