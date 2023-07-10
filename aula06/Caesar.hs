module Caesar where

import Data.Char (ord, chr)

encrypt :: Int -> String -> String
encrypt n s = map (encryptChar n) s

encryptChar :: Int -> Char -> Char
encryptChar n c = chr i'
  where
    i = ord c
    i' = (i + n) `mod` 256

decrypt :: Int -> String -> String
decrypt n s = encrypt (- n) s
