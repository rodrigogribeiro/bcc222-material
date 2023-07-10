-- |

module DataLens where

import Person

-- data representation for lenses

data Lens s a
  = Lens {
      _view :: s -> a
    , _set  :: a -> s -> s
    }

-- identity lens

idL :: Lens s s
idL = Lens id (\ _ s -> s)

-- composition

(@@) :: Lens s s1 -> Lens s1 a -> Lens s a
(Lens v1 s1) @@ (Lens v2 s2)
  = Lens (v2 . v1)
         (\ a s -> s1 (s2 a (v1 s)) s)

-- over operation: not efficient, because
-- it view and then update.
-- Ideally, we should do it in one step.

over :: Lens s a -> (a -> a) -> s -> s
over ln f s = _set ln (f (_view ln s)) s

-- lenses

street :: Lens Address String
street
  = Lens _street (\ s a -> a{ _street = s})

city :: Lens Address String
city
  = Lens _city (\ s a -> a{_city = s})

cep :: Lens Address String
cep
  = Lens _cep (\ s a -> a{_cep = s})

name :: Lens Person String
name
  = Lens _name (\ s p -> p{_name = s})

age :: Lens Person Int
age
  = Lens _age (\ i p -> p {_age = i})

address :: Lens Person Address
address
  = Lens _address (\ a p -> p {_address = a})


-- updating

setStreet :: String -> Person -> Person
setStreet
  = _set (address @@ street)

streetOf :: Person -> String
streetOf = _view (address @@ street)


-- lens para listas

setIdx :: Int -> a -> [a] -> [a]
setIdx _ _ [] = error "index too large"
setIdx i v (x : xs)
  | i < 0    = error "negative index"
  | otherwise = if i == 0
                then v : xs
                else x : setIdx (i - 1) v xs
                    
ix :: Int -> Lens [a] a
ix i = Lens (!! i) (setIdx i)

atPos :: (a -> a) -> Int -> [a] -> [a]
atPos f i xs
  = let
      v  = _view (ix i) xs
      v' = f v
    in _set (ix i) v' xs
