module Aula10 where

square :: Integer -> Integer
square n = n * n

if_ :: Bool -> a -> a -> a
if_ True  t _ = t
if_ False _ f = f

ones :: [Integer]
ones = 1 : ones

nats :: [Integer]
nats = 0 : map (+ 1) nats

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

primes :: [Integer]
primes = sieve [2 ..]
  where
    sieve (p : ns) = p : sieve [x | x <- ns, x `mod` p /= 0]

next :: Double -> Double -> Double
next n x0 = (x0 + n / x0) / 2

approximations :: Double -> Double -> [Double]
approximations x0 n = iterate (next n) x0

within :: Double -> [Double] -> Double
within phi (x0 : x1 : xs)
  | abs (x0 - x1) < phi = x1
  | otherwise           = within phi (x1 : xs)

newton :: Double -> Double
newton n = within 0.0001 (approximations (n / 2) n)
