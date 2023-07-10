{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Arith where

import Test.QuickCheck
import ParserLib

-- syntax of the data type

data Exp
   = Const Int
   | Exp :+: Exp
   | Exp :*: Exp
   deriving Show


-- definition of the parser

factorParser :: Parser Char Exp
factorParser
   =  parenthesized (expParser) <|>
      (Const <$> natural)

termParser :: Parser Char Exp
termParser
   = chainr factorParser pmult
     where
       pmult = const (:*:) <$> symbol '*'

expParser :: Parser Char Exp
expParser
  = chainr termParser pplus
    where
      pplus = const (:+:) <$> symbol '+'

-- definition of the interpreter

eval :: Exp -> Int
eval (Const n) = n
eval (e1 :+: e2) = eval e1 + eval e2
eval (e1 :*: e2) = eval e1 * eval e2


-- definition of the generation process

genExp :: Int -> Gen Exp
genExp h
  | h <= 1 = Const <$> choose (1,100)
  | otherwise = frequency [ (20, Const <$> choose (1,100))
                          , (40, (:+:) <$> genExp h2 <*> genExp h2)
                          , (40, (:*:) <$> genExp h2 <*> genExp h2)
                          ]
     where
       h2 = h `div` 2

instance Arbitrary Exp where
  arbitrary = sized genExp
