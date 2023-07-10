module Main (main) where

import ParserLib
import Arith
import Compiler
import StackVM

import System.Console.Haskeline



-- definition of a simple repl

repl :: IO ()
repl = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "exp> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just "quit" -> outputStrLn "Goodbye."
      Just inp ->
        case pipeline inp of
          Left err -> outputStrLn err >> loop
          Right v  -> outputStrLn (show v) >> loop

-- pipeline

pipeline :: String -> Either String Int
pipeline s
  = case parseExp s of
      Left err -> Left err
      Right e -> execute e

-- compile and execute code

execute :: Exp -> Either String Int
execute e
  = case interp (initialState (compile e)) of
      Just ((v : _), _) -> Right v
      _                 -> Left "Interpreter error!"

-- definition of a parser

parseExp :: String -> Either String Exp
parseExp s
  = case runParser expParser s of
      ((e, _) : _) -> Right e
      _            -> Left "Parse error!"


main :: IO ()
main = repl
