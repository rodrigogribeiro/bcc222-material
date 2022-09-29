data Nat = Zero | Succ Nat
           deriving (Eq, Ord, Show)

two :: Nat
two = Succ (Succ Zero)

(.+.) :: Nat -> Nat -> Nat
Zero      .+. m = m               -- 1
(Succ n') .+. m = Succ (n' .+. m) -- 2
