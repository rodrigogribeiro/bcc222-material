module Aula06 where

import Prelude hiding ( foldr
                      , foldl
                      , sum
                      , concat
                      , and
                      , length
                      , map
                      , reverse
                      , filter
                      )

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ v []       = v
foldr f v (x : xs) = x `f` foldr f v xs

sum = foldr (+) 0

concat = foldr (++) []

and = foldr (&&) True

length :: [a] -> Int
length = foldr step 0
   where
     step _ ac = 1 + ac

map :: (a -> b) -> [a] -> [b]
map f = foldr step []
   where
     step x ac = f x : ac

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ v []       = v
foldl f v (x : xs) = foldl f (f v x) xs

reverse = foldl (flip (:)) []

prod = foldl (*) 1

takeWhile1 :: (a -> Bool) -> [a] -> [a]
takeWhile1 p = foldl step base . reverse
  where
    base = []
    step ac x = if p x then x : ac else []



{-
prod [1,2,3] =
foldl (*) 1 [1,2,3] =
foldl (*) (1 * 1) [2,3] =
foldl (*) (1 * 2) [3] =
foldl (*) 2 [3] =
foldl (*) (2 * 3) [] =
foldl (*) 6 [] = 6

foldr (*) 1 [1,2,3] =
1 * (foldr (*) 1 [2, 3]) =
1 * (2 * (foldr (*) 1 [3])) =
1 * (2 * (3 * (foldr (*) 1 []))) =
1 * (2 * (3 * 1))

reverse [1,2,3] =
foldl (flip (:)) [] [1,2,3] =
foldl (flip (:)) (flip (:) [] 1) [2,3] =
foldl (flip (:)) [1] [2,3] =
foldl (flip (:)) (flip (:) [1] 2) [3] =
foldl (flip (:)) [2,1] [3] =
foldl (flip (:)) (flip (:) [2,1] 3) [] =
foldl (flip (:)) [3,2,1] [] =
[3,2,1]
-}


filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr step []
   where
     step x ac = if p x then x : ac else ac
