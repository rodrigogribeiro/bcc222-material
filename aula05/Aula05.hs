module Aula05 where

import Prelude hiding ( map
                      , filter
                      , foldr
                      , foldl
                      , flip
                      , (.)
                      )
import Data.Char

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

multThree :: Int -> Int -> Int -> Int
multThree x y z = x * y * z

doubleList' = map (\ x -> 2 * x)

greaterThan5 = filter (> 5)

(.) :: (b -> c) -> (a -> b) -> a -> c
g . f = \ x -> g (f x)

removeNull :: [[a]] -> [[a]]
removeNull = filter (not . null)

averageList :: [[Float]] -> [Float]
averageList = map average
    where
       average xs = sum xs / fromIntegral (length xs)

maxAverage :: [[Float]] -> Float
maxAverage = maximum . averageList . removeNull
