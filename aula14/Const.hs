module Const where

-- constant functor

newtype Const a b = Const a

getConst :: Const a b -> a
getConst (Const v) = v


instance Functor (Const a) where
  fmap _ (Const v) = Const v

instance Monoid a => Applicative (Const a) where
  pure _ = Const mempty
  (Const f) <*> (Const v) = Const (f `mappend` v)
