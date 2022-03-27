module Guess where

import Data.Char (isDigit)
import System.Environment (getArgs)


main :: IO ()
main = guessGame

-- error message

errorMsg :: String
errorMsg = "Entrada inválida! Digite dois inteiros positivos para jogar."

-- main game loop


guessGame :: IO ()
guessGame
  = do
      args <- getArgs
      case processArgs args of
        Bounds l u ->
          guess l u
        Invalid    ->
          putStrLn errorMsg

-- input argument processing.


data Arguments = Bounds Int Int  -- Correct input for the game
               | Invalid         -- invalid input


-- argument processing function

processArgs :: [String] -> Arguments 
processArgs (l : u : _)
  | all isDigit (l ++ u) = Bounds (read l) (read u) -- read :: Read a => String -> a
  | otherwise = Invalid
processArgs _ = Invalid


-- reading a guess from input

data Guess = Less | Greater | Equal

readGuess :: IO Guess
readGuess
  = do
      c <- getLine
      if c == "g" then return Greater
      else if c == "l" then return Less
           else return Equal

-- guessing game logic

guess :: Int -> Int -> IO ()
guess l u
  = do
      let m = (l + u) `div` 2
      putStr ("O número é " ++ show m ++ "?")
      putStrLn "(g = maior, l = menor, c = correto)"
      k <- readGuess
      case k of
        Less -> guess l (m - 1)
        Greater -> guess (m + 1) u
        Equal -> putStrLn "Acertou!"
