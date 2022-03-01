module Aula01 where

sumUpTo :: Int -> Int
sumUpTo 0 = 0
sumUpTo n = n + sumUpTo (n - 1)
