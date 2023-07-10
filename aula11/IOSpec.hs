module IOSpec where

import Prelude hiding (IO
                      , return
                      , (>>=)
                      , putChar
                      , putStr
                      )


type Console = [String]

type IO a = Console -> (a, Console)

runIO :: IO a -> Console -> (a, Console)
runIO prog console = prog console

putChar :: Char -> IO ()
putChar c = \ console -> ((), [c] : console)
 
return :: a -> IO a
return x = \ console -> (x, console)

(>>=) :: IO a -> (a -> IO b) -> IO b
m1 >>= f
  = \ console ->
      let
        (r, console') = runIO m1 console
        m2            = f r
      in runIO m2 console'

putStr :: String -> IO ()
putStr [] = return ()
putStr (c : cs)
  = do
      putChar c
      putStr cs
-- putChar c >>= \_ -> putStr cs
