module Aula12 where

mapMay :: (a -> b) -> Maybe a -> Maybe b
mapMay _ Nothing = Nothing
mapMay f (Just x) = Just (f x)

data Tree a
  = Leaf
  | Node a (Tree a) (Tree a)
  deriving (Eq, Ord, Show)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ Leaf = Leaf
mapTree f (Node x l r)
   = Node (f x) (mapTree f l) (mapTree f r)

data GRose f a = a :> f (GRose f a)

instance Functor Tree where
  fmap _ Leaf = Leaf
  fmap f (Node x l r)
    = Node (f x) (f <$> l) (f <$> r)

data Rose a
  = Rose a [Rose a]
    deriving (Eq, Ord, Show)

instance Functor Rose where
  -- fmap :: (c -> d)
  --      -> Rose c
  --      -> Rose d
  fmap f (Rose x ts)
    = Rose (f x) ((fmap f) <$> ts)

type From = Address
type To = Address

data Address
   = Address String
     deriving (Eq, Ord, Show)
data Body
   = Body String
     deriving (Eq, Ord, Show)

data Email
   = Email From To Body
     deriving (Eq, Ord, Show)

nonEmpty :: String -> Maybe String
nonEmpty [] = Nothing
nonEmpty s  = Just s

contains :: Char -> String -> Maybe String
contains x xs
  | x `elem` xs = Just xs
  | otherwise   = Nothing

mkAddress :: String -> Maybe Address
mkAddress s
  = Address <$> contains '@' s

mkBody :: String -> Maybe Body
mkBody s = Body <$> nonEmpty s

mkEmail :: String -> String -> String -> Maybe Email
mkEmail from to body
   = case mkAddress from of
       Nothing -> Nothing
       Just fromAddr ->
         case mkAddress to of
           Nothing -> Nothing
           Just toAddr ->
             case mkBody body of
               Nothing -> Nothing
               Just nBody ->
                   Just (Email fromAddr toAddr nBody)

mkEmail' :: String -> String -> String -> Maybe Email
mkEmail' from to body
   = Email <$> mkAddress from <*>
               mkAddress to   <*>
               mkBody body

data Validation err a
   = Failure err
   | Success a
   deriving (Eq, Ord, Show)

instance Functor (Validation err) where
   fmap _ (Failure err) = Failure err
   fmap f (Success x)   = Success (f x)

instance Semigroup err => Applicative (Validation err) where
  pure = Success
  Failure e1 <*> b = Failure $ case b of
    Failure e2 -> e1 <> e2
    Success _ -> e1
  Success _  <*> Failure e2 =
    Failure e2
  Success f  <*> Success a  =
    Success (f a)

data Error
  = MustNotBeEmpty
  | MustContain String
  deriving (Eq, Ord, Show)

atString :: String -> Validation [Error] Address
atString s
   | '@' `elem` s = Success (Address s)
   | otherwise    = Failure [MustContain "@"]

nonEmptyString :: String -> Validation [Error] Body
nonEmptyString [] = Failure [MustNotBeEmpty]
nonEmptyString s  = Success (Body s)

email :: String -> String ->
         String -> Validation [Error] Email
email from to body = Email <$> atString from <*>
                               atString to   <*>
                               nonEmptyString body
