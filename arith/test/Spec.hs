import Arith
import Compiler
import StackVM

import Test.QuickCheck

-- definition of the compiler correctness property

compileCorrect :: Exp -> Bool
compileCorrect e
  = case interp start of
      Just ((x : _), _) -> x == eval e
      _                 -> False
   where
     start = initialState (compile e)


main :: IO ()
main = quickCheckWith stdArgs {maxSuccess = 1000} compileCorrect
