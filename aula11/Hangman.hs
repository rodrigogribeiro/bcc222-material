module Hangman where

import Data.Char (isLetter)
import Data.Maybe (isNothing, isJust)
import System.IO
import System.Random (randomRIO)

-- main game function

configFile :: FilePath
configFile = "./data/words.txt"

tries :: Int
tries = 6

main :: IO ()
main
  = do
      content <- readFile configFile
      let
        bd = concatMap words (lines content)
      hSetBuffering stdout NoBuffering      -- força a impressão sem buffering
      hangman bd

-- main game logic

type BD = [String]

hangman :: BD -> IO ()
hangman [] = putStrLn "Empty word database."
hangman bd
  = do
       w <- selectWord bd
       let n = max tries (length w)
           st = zip w (repeat Nothing)
       startGameWithWord n st

selectWord :: BD -> IO String
selectWord bd
  = do
      i <- randomRIO (0, length bd - 1)
      return (bd !! i)

-- estado do jogo: uma lista de (Char, Maybe Char).
-- Quando o segundo componente é igual a Nothing,
-- então a letra ainda está oculta no jogo.

type Chances = Int

type GameState = [(Char, Maybe Char)]

startGameWithWord :: Chances -> GameState -> IO ()
startGameWithWord n s
  | n <= 0 = putStrLn "Você perdeu!"
  | otherwise
     = do
         showPartial s
         g <- readGuess
         case g of
           Letter c ->
             do 
               let s' = updateGameState c s
                   win = all (isJust . snd) s'
               if win then showPartial s' >> putStrLn "Você ganhou!"
               else startGameWithWord (n - 1) s'
           Invalid ->
             do
               putStrLn "Entrada inválida!"
               startGameWithWord n s

-- atualizando o estado com uma letra

updateGameState :: Char -> GameState -> GameState
updateGameState _ [] = []
updateGameState c ((c', p) : s')
  | c == c' && isNothing p
  = (c, Just c) : updateGameState c s'
  | otherwise = (c',p) : updateGameState c s'



-- mostrando acertos parciais

showPartial :: GameState -> IO ()
showPartial s = mapM_ step s >> putStrLn ""
   where
     step (_, Nothing)
       = putStr " _ "
     step (_, Just c)
       = putStr (" " ++ c : " ")


-- lendo uma tentativa

data Guess = Letter Char | Invalid

readGuess :: IO Guess
readGuess
  = do
       putStr "Digite uma letra:"
       s <- getLine
       case s of
         [ c ] -> if isLetter c then
                       return (Letter c)
                  else return Invalid
         _     -> return Invalid
