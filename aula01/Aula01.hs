module Aula01 where

sumUpTo :: Int -> Int
sumUpTo 0 = 0
sumUpTo n = n + sumUpTo (n - 1)

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

fact :: Int -> Int
fact n = n * factorial (n - 1)
fact 0 = 1

greet :: String -> String
greet name = "Hello, " ++ name ++ "!"

hello name = "Hello, " ++ name ++ "!"
