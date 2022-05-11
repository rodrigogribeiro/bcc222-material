{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, GADTs #-}
module Free where

import Data.Char


data Validation err a
  = Failure err
  | Success a


instance Functor (Validation err) where
  fmap f g@(Failure e) = Failure e
  fmap f (Success x)   = Success (f x)

instance Semigroup err => Applicative (Validation err) where
  pure = Success
  (Failure e1) <*> (Failure e2) = Failure (e1 <> e2)
  (Failure e1) <*> _            = Failure e1
  (Success f)  <*> (Success x)  = Success (f x)

type Reader err a = IO (Validation err a)
type Label = String


reader :: Label -> (String -> Validation err a) -> Reader err a
reader lbl f
  = do
      putStr lbl
      f <$> getLine

int :: Label -> Reader err Int
int lbl = reader lbl f
  where
    f s = if all isDigit s then Success (read s)
             else 
