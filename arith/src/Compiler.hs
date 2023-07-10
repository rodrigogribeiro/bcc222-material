{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Compiler where

import Arith
import StackVM

-- definition of the compiler

compile :: Exp -> Code
compile (Const n) = [PUSH n]
compile (e1 :+: e2)
  = compile e1 ++ compile e2 ++ [ADD]
compile (e1 :*: e2)
  = compile e1 ++ compile e2 ++ [MUL]
