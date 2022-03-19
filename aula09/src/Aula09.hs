module Aula09 where

import Data.Char
import Test.Tasty
import Test.Tasty.HUnit      as TH
import Test.Tasty.QuickCheck as TQ
import Test.QuickCheck

data Name = MkName String deriving Eq

mkName :: String -> Name
mkName []       = MkName []
mkName (x : xs) = MkName (x' : xs')
  where
     x'  = toUpper x
     xs' = map toLower xs

render :: Name -> String
render (MkName s) = s

mkNameTest :: TestTree
mkNameTest
  = testCase "MkName Test" (s @?= "Maria")
  where s = render (mkName "maria")

startsWithUpper :: String -> Bool
startsWithUpper [] = True
startsWithUpper (c : _) = isUpper c

nameCorrect :: String -> Bool
nameCorrect s = startsWithUpper (render (mkName s))

implies :: Bool -> Bool -> Bool
implies x y = not x || y

nameCorrectFixed :: String -> Bool
nameCorrectFixed s = (all isLetter s) `implies` b
   where
     b  = startsWithUpper s'
     s' = render (mkName s)

sort1 :: [Int] -> [Int]
sort1 []       = []
sort1 (x : xs) = insert1 x xs

insert1 :: Int -> [Int] -> [Int]
insert1 x [] = [x]
insert1 x (y : ys)
      | x <= y    = x : ys
      | otherwise = y : insert1 x ys

sortPreservesLength :: [Int] -> Bool

insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x (y : ys)
      | x <= y    = x : y : ys
      | otherwise = y : insert x ys

preserves :: Eq b => (a -> a) -> (a -> b) -> a -> Bool
(f `preserves` p) x = p x == p (f x)

sortPreservesLength = sort `preserves` length

idPreservesLength :: [Int] -> Bool
idPreservesLength = id `preserves` length

sortEnsuresSorted :: [Int] -> Bool
sortEnsuresSorted = sorted . sort

sorted :: [Int] -> Bool
sorted []  = True
sorted [_] = True
sorted (x : y : ys) = x <= y && sorted (y : ys)

evilSort :: [Int] -> [Int]
evilSort xs = replicate (length xs) 1

permutes :: ([Int] -> [Int]) -> [Int] -> Bool
permutes f xs =  all (flip elem xs) (f xs)

sortPermutes :: [Int] -> Bool
sortPermutes xs = sort `permutes` xs

inRange :: Int -> Int -> [Int] -> [Int]
inRange = undefined

inRangeUnit :: TestTree
inRangeUnit
   = testCase "inRage test" $
       inRange 2 5 [1..10] @?= [2,3,4,5]

inRangeProperty :: Int -> Int -> [Int] -> Bool
inRangeProperty _ _ _ = True

properties :: TestTree
properties
    = testGroup "Propriedades"
         [
            TQ.testProperty "sort preserves length"
               sortPreservesLength
         ,  TQ.testProperty "id preserves length"
               idPreservesLength
         ,  TQ.testProperty "sort ensures sorting"
               sortEnsuresSorted
         ,  TQ.testProperty "sort permutes input"
               sortPermutes
         ,  TQ.testProperty "evil sort ensures sorting"
               (sorted . evilSort)
         ,  TQ.testProperty "inRange specification"
               inRangeProperty
         ]

unitTests :: TestTree
unitTests = testGroup "Testes de unidade"
                      [
                        mkNameTest
                      , inRangeUnit
                      ]

tests :: TestTree
tests = testGroup "Testes" [unitTests, properties]


main :: IO ()
main = defaultMain tests

sort :: [Int] -> [Int]
sort [] = []
sort (x : xs) = insert x (sort xs)
