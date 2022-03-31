module Aula14 where

import Control.Monad
import Control.Monad.Fail

data Exp
   = Const Int
   | Exp :+: Exp
   | Exp :*: Exp
   deriving (Eq, Ord, Show)

eval :: Exp -> Int
eval (Const n)
   = n
eval (e :+: e')
   = eval e + eval e'
eval (e :*: e')
   = eval e * eval e'

data Exp1
  = Const1 Int
  | Exp1 :++: Exp1
  | Exp1 :**: Exp1
  | Exp1 :/: Exp1
  deriving (Eq, Ord, Show)

eval1 :: Exp1 -> Maybe Int
eval1 (Const1 n)
   = Just n
eval1 (e :++: e')
  = case eval1 e of
      Just n ->
        case eval1 e' of
          Just m -> Just (n + m)
          Nothing -> Nothing
      Nothing -> Nothing

eval1 (e :**: e')
  = case eval1 e of
      Just n ->
        case eval1 e' of
          Just m -> Just (n * m)
          Nothing -> Nothing
      Nothing -> Nothing

eval1 (e :/: e')
  = case eval1 e of
      Just n ->
        case eval1 e' of
          Just m -> if m == 0 then Nothing
                    else Just (n `div` m)
          Nothing -> Nothing
      Nothing -> Nothing

(>>?) :: Maybe a -> (a -> Maybe b) -> Maybe b
(Just v) >>? f = f v
Nothing  >>? _ = Nothing

eval2 :: Exp1 -> Maybe Int
eval2 (Const1 n) = Just n
eval2 (e :++: e') =
   eval2 e >>? (\ n ->
      eval2 e' >>? \m -> Just (n + m))

eval2 (e :**: e') =
   eval2 e >>? (\ n ->
       eval2 e' >>? \m -> Just (n * m))
eval2 (e :/: e') =
   eval2 e >>? (\ n ->
       eval2 e' >>? \m -> if m == 0
                          then Nothing
                          else Just (n `div` m))

eval3 :: (Monad m, MonadFail m) => Exp1 -> m Int
eval3 (Const1 n) = return n
eval3 (e :++: e')
  = eval3 e >>= \ n ->
    eval3 e' >>= \ m ->
    return (n + m)

eval3 (e :**: e')
  = eval3 e >>= \ n ->
    eval3 e' >>= \ m ->
    return (n * m)
eval3 (e :/: e')
  = eval3 e >>= \ n ->
    eval3 e' >>= \ m ->
    if m == 0 then fail "Division by zero"
      else return (n `div` m)

eval4 :: (Monad m, MonadFail m) => Exp1 -> m Int
eval4 (Const1 n) = return n
eval4 (e :++: e')
  = do
       n <- eval4 e
       m <- eval4 e'
       return (n + m)

eval4 (e :**: e')
  = do
       n <- eval4 e
       m <- eval4 e'
       return (n * m)
eval4 (e :/: e')
  = do
       n <- eval4 e
       m <- eval4 e'
       if m == 0 then fail "Division by zero"
         else return (n `div` m)

triples :: Int -> [(Int,Int,Int)]
triples n
  = do
       x <- [1..n]
       y <- [1..n]
       z <- [1..n]
       guard (x^2 == y^2 + z^2)
       return (x,y,z)

digits :: [Int]
digits = [0..9]

toNumber :: [Int] -> Int
toNumber = foldl (\ ac d -> ac * 10 + d) 0

remove :: Eq a => [a] -> [a] -> [a]
remove rs ls = foldl remove' ls rs
      where
        remove' ls x = filter (/= x) ls

solutions :: [(Int,Int,Int)]
solutions = do
   s <- remove [0] digits
   e <- remove [s] digits
   n <- remove [s,e] digits
   d <- remove [s,e,n] digits
   let send = toNumber [s, e, n, d]
   m <- remove [0,s,e,n,d] digits
   o <- remove [s,e,n,d,m] digits
   r <- remove [s,e,n,d,m,o] digits
   let more = toNumber [m, o, r, e]
   y <- remove [s,e,n,d,m,o,r] digits
   let money = toNumber [m,o,n,e,y]
   guard $ send + more == money
   return (send, more, money)

data Tree a
  = Leaf a
  | Node (Tree a) (Tree a)
  deriving (Eq, Ord, Show)

t1 :: Tree Char
t1 =  Node (Node (Leaf 'a') (Leaf 'b'))
           (Leaf 'c')

t2 :: Tree (Char, Int)
t2 = Node (Node (Leaf ('a',0)) (Leaf ('b',1)))
          (Leaf ('c',2))

label :: Tree a -> Tree (a,Int)
label = fst . flip labelAcc 0
   where
     labelAcc (Leaf x) n
        = (Leaf (x,n) , n + 1)
     labelAcc (Node tl tr) n
        = (Node tl' tr' , n2)
          where
            (tl', n1) = labelAcc tl n
            (tr', n2) = labelAcc tr n1

newtype State s a
   = State { runState :: s -> (a, s) }

put :: s -> State s ()
put s = State (\ _ -> ((), s))

get :: State s s
get = State (\ s -> (s, s))

instance Functor (State s) where
   fmap f (State g)
     = State (\ s ->
          let (v, s') = g s
          in (f v, s'))

instance Applicative (State s) where
   pure v = State (\ s -> (v, s))
   (State f) <*> (State g)
     = State (\s -> let (h, s1) = f s
                        (v, s2) = g s1
                    in (h v, s2))

instance Monad (State s) where
   return = pure
   (State m) >>= f
      = State (\ s -> let (v, s') = m s
                      in runState (f v) s')

instance MonadFail (State s) where
   fail s = error s

fresh :: State Int Int
fresh
   = do
       n <- get
       put (n + 1)
       return n

lbl :: Tree a -> Tree (a, Int)
lbl t = fst (runState (mk t) 0)
  where
    mk (Leaf v)
      = do
          n <- fresh
          return (Leaf (v, n))
    mk (Node tl tr)
      = do
          tl' <- mk tl
          tr' <- mk tr
          return (Node tl' tr')
