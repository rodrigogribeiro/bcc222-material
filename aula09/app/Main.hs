{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Test.Tasty
import Test.Tasty.QuickCheck



main :: IO ()
main =  defaultMain tests

tests :: TestTree
tests = testGroup "testes"
            [
              testProperty "reverse" $
                 \ (xs :: [Int]) ->
                      reverse (reverse xs) == xs
            ]
