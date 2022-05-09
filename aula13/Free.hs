{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, GADTs #-}
module Free where

import Data.Char


data FreeA f a
  = Pure a
  | forall b. f (b -> a) :$: (FreeA f b)

instance Functor f => Functor (FreeA f) where
  fmap g (Pure x) = Pure (g x)
  fmap g (h :$: x) = fmap (g .) h :$: x


instance Functor f => Applicative (FreeA f) where
  pure = Pure
  (Pure f) <*> y = f <$> y
  (h :$: x) <*> y = (fmap uncurry h) :$: ((,) <$> x <*> y)
