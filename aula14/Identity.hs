module Identity where

newtype Identity a = Identity a


runIdentity :: Identity a -> a
runIdentity (Identity v) = v


instance Functor Identity where
  fmap f (Identity v)
    = Identity (f v)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity v)
    = Identity (f v)
