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
                      , (++)
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

reverse = foldl step []
   where
     step ac x = x : ac

filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr step []
   where
     step x ac = if p x then x : ac else ac
