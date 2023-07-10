module Aula05 where

import Prelude hiding ( map
                      , filter
                      , foldr
                      , foldl
                      , flip
                      , (.)
                      )
import Data.Char

power :: Int -> Int -> Int
power _ 0 = 1
power n m = n * power n (m - 1)

powerTwo :: Int -> Int
powerTwo n = power 2 n

position :: Eq a => a -> [a] -> Maybe Int
position x xs = aux x xs 0
  where
    aux _ [] _ = Nothing
    aux x (y : ys) ac
      | x == y = Just ac
      | otherwise = aux x ys (ac + 1)

indexOf :: Int -> [Int] -> Int
indexOf x xs = index' x xs 0
  where
    index' _ [] _ = -1
    index' x (y : ys) ac
      | x == y = ac
      | otherwise = index' x ys (ac + 1)

quad :: Int -> Int
quad b = power b 2

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
