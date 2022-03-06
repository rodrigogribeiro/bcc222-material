module Aula05 where

import Prelude hiding ( map
                      , filter
                      , foldr
                      , foldl
                      )

map :: (a -> b) -> [a] -> [b]
map _ []       = []
map f (x : xs) = f x : map f xs

doubleList :: [Int] -> [Int]
doubleList xs = map double xs
     where
       double x = 2 * x

notList :: [Bool] -> [Bool]
notList xs = map not xs

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x : xs)
  | p x = x : filter p xs
  | otherwise = filter p xs

lowers :: String -> String
lowers xs = filter isLower xs

evens :: [Int] -> [Int]
evens xs = filter even xs
