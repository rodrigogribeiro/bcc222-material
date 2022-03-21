module Aula02 where
import Prelude hiding ( replicate
                      , null
                      )

average vs = sum vs `div` length ns

add1 :: Int -> Int
add1 x = x + 1

median :: Float -> Float -> Float
median x y = (x + y) / 2

sum3 :: Int -> Int -> Int -> Int
sum3 x y z = x + y + z

dist :: (Float, Float) -> (Float, Float) -> Float
dist (x1,y1) (x2,y2)
   = sqrt ((x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2))

abs1 :: Int -> Int
abs1 n = if n < 0 then (- 1) * n else n

signal1 :: Int -> Int
signal1 n = if n < 0 then -1
            else if n == 0 then 0
                 else 1

abs2 :: Int -> Int
abs2 n
  | n < 0 = n * (- 1)
  | otherwise = n

signal2 :: Int -> Int
signal2 n
  | n < 0 = -1
  | n == 0 = 0
  | otherwise = 1

dist1 :: (Float,Float) -> (Float,Float) -> Float
dist1 (x1,y1) (x2,y2)
    = let
        dx  = x1 - x2
        dx2 = dx * dx
        dy  = y1 - y2
        dy2 = dy * dy
      in sqrt (dx2 + dy2)

dist2 :: (Float,Float) -> (Float,Float) -> Float
dist2 (x1,y1) (x2,y2)
  = sqrt (dx2 + dy2)
    where
     dx  = x1 - x2
     dx2 = dx * dx
     dy  = y1 - y2
     dy2 = dy * dy

-- Euler constant 
e :: Float
e = 2.718

{-
 Definindo uma lista vazia
-}
empty :: [a]
empty = []

fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n - 1)

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x : replicate (n - 1) x

conj :: Bool -> Bool -> Bool
conj False False = False
conj False True  = False
conj True  False = False 
conj True  True = True

conj1 :: Bool -> Bool -> Bool
conj1 True  v = v
conj1 False _ = False

null :: [a] -> Bool
null []      = True
null (_ : _) = False

trim :: String -> String
trim (' '  : s) = trim s
trim ('\t' : s) = trim s
trim s = s

bothZero :: (Int,Int) -> Bool
bothZero (0,0) = True
bothZero _     = False

sumIfThree :: [Int] -> Int
sumIfThree (a : b : c : []) = a + b + c
sumIfThree _                = 0

size :: [a] -> Int
size []       = 0
size (_ : xs) = 1 + size xs
