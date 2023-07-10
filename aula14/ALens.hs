{-# LANGUAGE RankNTypes #-}

module ALens where

import Person
import Identity
import Const

-- definition of applicative lenses

type ALens s a
  = forall f. Applicative f => (a -> f a) -> s -> f s

-- Same definition for lens!

set :: ALens s a -> a -> s -> s
set ln a s
  = runIdentity (ln set_fld s)
  where
    set_fld _ = Identity a


over :: ALens s a -> (a -> a) -> s -> s
over ln f s
  = runIdentity (ln (Identity . f) s)


view :: Monoid a => ALens s a -> s -> a
view ln s
  = getConst (ln Const s)


infixl 8 ^.

(^.) :: Monoid a => s -> ALens s a -> a
s ^. ln = view ln s

(~.) :: ALens s a -> a -> s -> s
(~.) = set


address :: ALens Person Address
address f (Person n a ad)
  = (\ ad' -> Person n a ad') <$> f ad

-- the lens for street and city

streetCity :: ALens Address String
streetCity f (Address st ct cp)
  = (\ st' ct' -> Address st' ct' cp) <$>
        f st <*> f ct
