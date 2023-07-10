{-# LANGUAGE InstanceSigs #-}
module Aula08 where

data Point = Point Int Int

instance Eq Point where
   (Point x y) == (Point x' y') =
      x == x' && y == y'
   x /= y = not (x == y)

data Vec3 = Vec3 Int Int Int

data BoolPair = BoolPair Bool Bool

instance Eq BoolPair where
   (==) :: BoolPair -> BoolPair -> Bool
   (BoolPair b1 b2) == (BoolPair b1' b2')
      = b1 == b1' && b2 == b2'
   (/=) :: BoolPair -> BoolPair -> Bool
   b1 /= b2 = not (b1 == b2)

data Point2D
   = Point2D Int Int
     deriving (Eq, Ord, Show)

data Vector
   = Vector Float Float
     deriving (Eq, Ord, Show)

data Shape
   = Rectangle Vector Float Float
   | Circle Vector Float
   deriving (Eq, Ord, Show)

class Scale a where
   scale :: Float -> a -> a

instance Scale Vector where
   scale s v@(Vector x y)
      = Vector (x * s') (y * s')
        where
         s' = s / norm v
         norm (Vector a b) = sqrt (a^2 + b^2)

instance Scale Shape where
   scale s (Circle p r)      = Circle p (s * r)
   scale s (Rectangle p w h) = Rectangle p (s * w) (s * h)

double :: Scale a => a -> a
double s = scale 2.0 s

instance Scale a => Scale [a] where
   scale s = map (scale s)

data Exp
  = Const Int
  | Exp :+: Exp
  | Exp :-: Exp
  | Exp :*: Exp
  | Abs Exp
  | Sign Exp
  deriving (Eq, Ord, Show)

ex :: Exp
ex = Const 1 :+: Const 1

instance Num Exp where
  fromInteger = Const . fromInteger
  (+) = (:+:)
  (-) = (:-:)
  (*) = (:*:)
  abs = Abs
  signum = Sign

ex1 :: Exp
ex1 = 1 + 1

data Direction
   = North | South | East | West
     deriving (Eq, Ord, Show, Enum)

data Person
  = Person {
      name :: String
    , age  :: Int
    }
