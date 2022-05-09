{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, GADTs #-}
module Parsing where

import Data.Char


newtype Parser s a
  = Parser {runParser :: [s] -> [(a,[s])]}

symbol :: Eq s => s -> Parser s s
symbol s
  = Parser (\ inp ->
              case inp of
                [] -> []
                (x : xs) -> if s == x
                            then [(s, xs)]
                            else [])
sat :: (s -> Bool) -> Parser s s
sat p
  = Parser (\ inp ->
              case inp of
                [] -> []
                (x : xs) -> if p x
                            then [(x,xs)]
                            else []) 

digitChar :: Parser Char Char
digitChar = sat isDigit

instance Functor (Parser s) where
  fmap f (Parser p)
    = Parser (\ inp ->
        [(f x, xs) | (x,xs) <- p inp])

digit :: Parser Char Int
digit = f <$> digitChar
  where
    f c = ord c - ord '0'

(<|>) :: Parser s a ->
         Parser s a ->
         Parser s a
p1 <|> p2
  = Parser (\inp -> runParser p1 inp ++
                    runParser p2 inp)

succeed :: a -> Parser s a
succeed x = Parser (\ inp -> [(x, inp)])

failure :: Parser s a
failure = Parser (\ _ -> [])

instance Applicative (Parser s) where
  pure = succeed
  p1 <*> p2
    = Parser (\ inp ->
        [(f x, r2) | (f, r1) <- runParser p1 inp,
                     (x, r2) <- runParser p2 r1])


data CEP = CEP String String

instance Show CEP where
  show (CEP s1 s2) = s1 ++ "-" ++ s2


repeatParser :: Int -> Parser s a -> Parser s [a]
repeatParser n p
  | n <= 0 = succeed []
  | otherwise = (:) <$> p <*> repeatParser (n - 1) p

cepParser :: Parser Char CEP
cepParser = (\ s1 _ s2 -> CEP s1 s2)   <$>
            (repeatParser 5 digitChar) <*>
            symbol '-'                 <*>
            (repeatParser 3 digitChar)


many :: Parser s a -> Parser s [a]
many p = ((:) <$> p <*> many p) <|> succeed []

first :: Parser s a -> Parser s a
first p
  = Parser (\ inp ->
              case runParser p inp of
                [] -> []
                (x : _) -> [x])

greedy :: Parser s a -> Parser s [a]
greedy = first . many

natural :: Parser Char Int
natural = f <$> greedy digit
  where
    f = foldl step 0
    step b a = b * 10 + a


