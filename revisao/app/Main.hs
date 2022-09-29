module Main where

import System.Environment (getArgs)

run :: String -> IO ()
run s
  = do
      content <- readFile s
      let
        fileLines = lines content
        fileWords = concatMap words fileLines
      putStrLn $ "Linhas:" ++ (show $ length fileLines)
      putStrLn $ "Palavras:" ++ (show $ length fileWords)

main :: IO ()
main
  = do
      args <- getArgs
      case args of
        (f : _) -> run f
        []      ->
          putStrLn "Specify a file"
