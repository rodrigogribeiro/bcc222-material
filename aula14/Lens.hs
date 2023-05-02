{-# LANGUAGE RankNTypes #-}

module Lens where

import qualified DataLens as D
import Identity
import Const
import Person
import Data.Function

type Lens s a
  = forall f. Functor f =>
      (a -> f a) -> s -> f s


set :: Lens s a -> a -> s -> s
set ln a s
  = runIdentity (ln set_fld s)
  where
    set_fld _ = Identity a


over :: Lens s a -> (a -> a) -> s -> s
over ln f s
  = runIdentity (ln (Identity . f) s)


view :: Lens s a -> s -> a
view ln s
  = getConst (ln Const s)


lens2lensD :: Lens s a -> D.Lens s a
lens2lensD ln
  = D.Lens (view ln)
           (set ln)


lensD2lens :: D.Lens s a -> Lens s a
lensD2lens (D.Lens vw st) trans s
  = (flip st s) <$> trans (vw s)


lens :: (s -> a) -> (a -> s -> s) -> Lens s a
lens vw st trans s
  = flip st s <$> trans (vw s)

-- a simple operator for view


infixl 8 ^.

(^.) :: s -> Lens s a -> a
s ^. ln = view ln s

-- operator for set

(~.) :: Lens s a -> a -> s -> s
(~.) = set

-- lenses for person data

name :: Lens Person String
name
  = lens _name setName
    where
      setName n (Person _ ag ad)
        = Person n ag ad

age :: Lens Person Int
age
  = lens _age setAge
    where
      setAge i (Person n _ ad)
        = Person n i ad

address :: Lens Person Address
address
  = lens _address setAddress
    where
      setAddress ad (Person n i _)
        = Person n i ad


street :: Lens Address String
street
  = lens _street setStreet
    where
      setStreet s (Address _ c cp)
        = Address s c cp
