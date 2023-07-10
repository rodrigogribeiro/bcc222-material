module Extra01 where

import Data.Char (chr, ord, isLower)

char2Int :: Char -> Int
char2Int c = ord c - ord 'a'

int2Char :: Int -> Char
int2Char n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c
  | isLower c = int2Char ((char2Int c + n) `mod` 26)
  | otherwise = c

encrypt :: Int -> String -> String
encrypt n = map (shift n)

decrypt :: Int -> String -> String
decrypt n = encrypt (- n)

type Bit = Int

bin2Int :: [Bit] -> Int
bin2Int bs = sum [w * b | (w,b) <- zip weights bs]
        where
          weights = iterate (* 2) 1

int2Bin :: Int -> [Bit]
int2Bin 0 = []
int2Bin n = n `mod` 2 : int2Bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bs = take 8 (bs ++ repeat 0)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bs = take 8 bs : chop8 (drop 8 bs)

encode :: String -> [Bit]
encode = concat . map (make8 . int2Bin . ord)

decode :: [Bit] -> String
decode = map (chr . bin2Int) . chop8
