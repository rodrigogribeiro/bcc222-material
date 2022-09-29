{-# LANGUAGE RankNTypes#-}

module LensExamples where

import Lens
import Data.Function
import Data.Map (Map)
import qualified Data.Map as Map

-------------------------------
-- example 1: virtual fields --
-------------------------------


data Temp
  = Temp {
      _celsius :: Float
    } deriving Show

celsius :: Lens Temp Float
celsius f (Temp c)
  = Temp <$> (f c)

-- virtual field for farenheint

--Farenheit to Celsius

ftc :: Float -> Float
ftc = (*(5/9)) . ((+) (-32))

--To Farenheit
--Celsius to Farenheit

ctf :: Float -> Float
ctf = (+32) . (*(9/5))

farenheint :: Lens Temp Float
farenheint f (Temp c)
  = (\ fa -> Temp (ftc fa)) <$> (f (ctf c))

-------------------------------
-- example 2: Invariants     --
-------------------------------

data Time
  = Time {
      _hours :: Int
    , _mins :: Int
    } deriving Show


mins :: Lens Time Int
mins f (Time h m)
  = wrap <$> (f m)
  where
    wrap :: Int -> Time
    wrap m'
      | m' >= 60  = Time (h + 1) (m' - 60)
      | m' < 0    = Time (h - 1) (m' + 60)
      | otherwise = Time h m'

now :: Time
now = Time 3 58

now' :: Time
now' = over mins (+ 4) now

-------------------------------
-- example 3: Indexed lens   --
-------------------------------

at :: Ord k => k -> Lens (Map k v) (Maybe v)
at k f m
  = wrap <$> (f v)
    where
      v = Map.lookup k m

      wrap (Just v') = Map.insert k v' m
      wrap Nothing
        = case v of
            Nothing -> m
            Just _  -> Map.delete k m
