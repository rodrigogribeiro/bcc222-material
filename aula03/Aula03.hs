-- |
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Aula03 where

foo :: [String] -> String
foo [] = 1
foo (x : xs) = x * foo xs

instance Num String where
  x * y = show $ (length x) * (length y)
