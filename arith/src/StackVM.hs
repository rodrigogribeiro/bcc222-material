{-# OPTIONS_GHC -Wno-missing-export-lists -Wno-name-shadowing #-}
module StackVM where

-- definition of the instructions

data Instr
  = ADD
  | MUL
  | PUSH Int
  deriving Eq

-- definition of a program

type Code = [Instr]

-- definition of the machine state

type Stack = [Int]

type State = (Stack, Code)

-- defining the initial state

initialState :: [Instr] -> State
initialState is
  = ([], is)

-- definition of the interpreter

interp :: State -> Maybe State
interp s@(_, []) = Just s
interp (stk, (i:is))
  = case i of
      PUSH n -> interp (n : stk , is)
      _      -> evalOp i stk is
   where
     evalOp i (v1 : v2 : stk') is
       = if i == ADD then interp (v1 + v2 : stk' , is)
         else interp (v1 * v2 : stk' , is)
     evalOp _ _ _ = Nothing

