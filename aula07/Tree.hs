module Tree where

import Prelude hiding (elem)

data Tree a
  = Leaf
  | Node a (Tree a) (Tree a)
  deriving Show

ex :: Tree Int
ex = Node 2 (Node 1 Leaf Leaf)
            (Node 3 Leaf Leaf)
 
elem :: Ord a => a -> Tree a -> Bool
elem _ Leaf = False
elem x (Node y l r)
  = case compare x y of
      LT -> elem x l
      EQ -> True 
      GT -> elem x r

insert :: Ord a => a -> Tree a -> Tree a
insert x Leaf = Node x Leaf Leaf
insert x tree@(Node y l r)
  = case compare x y of
      LT -> Node y (insert x l) r
      GT -> Node y l (insert x r)
      _  -> tree 

toList :: Tree a -> [a]
toList Leaf = []
toList (Node y l r)
  = toList l ++ [y] ++ toList r

fromList :: Ord a => [a] -> Tree a
fromList = foldr insert Leaf

treeSort :: Ord a => [a] -> [a]
treeSort = toList . fromList

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

data Exp
  = Const Int
  | Exp :+: Exp
  | Exp :*: Exp
  | Exp :/: Exp
  deriving Show

eval :: Exp -> Int
eval (Const n) = n
eval (e1 :+: e2) = eval e1 + eval e2
eval (e1 :*: e2) = eval e1 * eval e2
eval (e1 :/: e2) = let
                     v2 = eval e2
                   in if v2 == 0 then error "erro"
                        else eval e1 `div` v2

eval1 :: Exp -> Maybe Int
eval1 (Const n) = Just n
eval1 (e1 :+: e2)
  = case eval1 e1 of
      Just v1 ->
        case eval1 e2 of
          Just v2 -> Just (v1 + v2)
          Nothing -> Nothing
      Nothing -> Nothing

heads :: [[a]] -> [a]
heads xss = [x | (x : _) <- xss]
