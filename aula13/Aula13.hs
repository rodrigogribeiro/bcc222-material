{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Aula13 where

import Data.Char

newtype Parser s a
     = Parser {
         runParser :: [s] -> [(a,[s])]
       }

symbol :: Eq s => s -> Parser s s
symbol s = Parser (\ inp -> case inp of
                              [] -> []
                              (x : xs) -> if x == s
                                          then [(s,xs)]
                                          else [])

token :: Eq s => [s] -> Parser s [s]
token s
  = Parser (\ inp -> if s == (take n inp)
                     then [(s, drop n inp)]
                     else [])
    where
      n = length s

sat :: (s -> Bool) -> Parser s s
sat p = Parser (\ inp -> case inp of
                           [] -> []
                           (x : xs) -> if p x
                                       then [(x,xs)]
                                       else [])

digitChar :: Parser Char Char
digitChar = sat isDigit

instance Functor (Parser s) where
   fmap f (Parser p)
      = Parser (\ inp -> [(f x, xs) | (x,xs) <- p inp])

digit :: Parser Char Int
digit = f <$> digitChar
        where
          f c = ord c - ord '0'

succeed :: a -> Parser s a
succeed v = Parser (\ inp -> [(v,inp)])

failure :: Parser s a
failure = Parser (\ _ -> [])

infixr 4 <|>

(<|>) :: Parser s a -> Parser s a -> Parser s a
(Parser p) <|> (Parser q)
   = Parser (\ inp -> p inp ++ q inp)

instance Applicative (Parser s) where
   pure = succeed
   (Parser p) <*> (Parser q)
     = Parser (\ inp -> [(f x, xs) | (f, ys) <- p inp
                                   , (x, xs) <- q ys])

ex1 :: Parser Char String
ex1 = token "ab" <|> token "ba"

data Paren = Match Paren Paren | Empty
             deriving Show

open :: Parser Char Char
open = symbol '('

close :: Parser Char Char
close = symbol ')'

parens :: Parser Char Paren
parens = (f <$> open <*> parens
                     <*> close
                     <*> parens)
         <|> succeed Empty
         where
           f _ p _ p' = Match p p'

option :: Parser s a -> a -> Parser s a
option p d = p <|> succeed d

many :: Parser s a -> Parser s [a]
many p = ((:) <$> p <*> many p) <|> succeed []

many1 :: Parser s a -> Parser s [a]
many1 p = (:) <$> p <*> many p

natural :: Parser Char Int
natural = foldl f 0 <$> many digit
     where
       f ac d = ac * 10 + d

first :: Parser s a -> Parser s a
first (Parser p)
   = Parser (\ inp -> let r = p inp
                      in if null r then []
                         else [head r])

greedy :: Parser s a -> Parser s [a]
greedy = first . many

greedy1 :: Parser s a -> Parser s [a]
greedy1 = first . many1

identifier :: Parser Char String
identifier
   = (:) <$> letter <*> greedy (sat isAlphaNum)
     where
       letter = sat isLetter

listOf :: Parser s a -> Parser s b -> Parser s [a]
listOf p sep
   = (:) <$> p <*> many ((\ x y -> y) <$> sep <*> p)

pack :: Parser s a -> Parser s b ->
        Parser s c -> Parser s b
pack p q r = (\ _ x _ -> x) <$> p <*> q <*> r

parenthesized :: Parser Char a -> Parser Char a
parenthesized p = pack (symbol '(') p (symbol ')')

endBy :: Parser s a -> Parser s b -> Parser s [a]
endBy p sep = greedy ((\ x _ -> x) <$> p <*> sep)

type CSV = [Line]
type Line = [Cell]
type Cell = String

cellParser :: Parser Char Cell
cellParser = greedy valid
    where
       valid = sat (\ c -> notElem c ",\n")

lineParser :: Parser Char Line
lineParser = listOf cellParser (symbol ',')

csvParser :: Parser Char CSV
csvParser = endBy lineParser eol
   where
     eol = symbol '\n'

parseCSV :: FilePath -> IO ()
parseCSV file
   = do
       content <- readFile file
       print (runParser csvParser content)

chainr :: Parser s a ->             -- expressão
          Parser s (a -> a -> a) -> -- operador
          Parser s a
chainr pe po
   = h <$> many (j <$> pe <*> po) <*> pe
     where
       j x op = op x
       h fs x = foldr ($) x fs

chainl :: Parser s a ->             -- expressão
          Parser s (a -> a -> a) -> -- operador
          Parser s a
chainl pe po
   = h <$> pe <*> many (j <$> po <*> pe)
     where
       j op x = \ y -> op y x
       h x fs = foldl (flip ($)) x fs

data Exp
   = Const Int
   | Exp :+: Exp
   | Exp :*: Exp
   deriving (Eq, Ord, Show)

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
