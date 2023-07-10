module Main where

import Data.List (intersperse, maximumBy)
import Data.Maybe (isJust)

-- rose trees

data Rose a = a :> [Rose a]
              deriving (Show, Eq, Ord)

test :: Rose Int
test = 1 :> [ 2 :> []
            , 3 :> []
            , 4 :> [
                     5 :> []
                   , 6 :> []
                   ]
            ]

-- function for root

root :: Rose a -> a
root (x :> _) = x

-- function for children

children :: Rose a -> [Rose a]
children (_ :> xs) = xs

-- number of nodes in a rose tree

size :: Rose a -> Int
size (_ :> xs) = 1 + sum (map size xs)

-- number of leaves

leaves :: Rose a -> Int
leaves (_ :> []) = 1
leaves (_ :> xs) = sum (map leaves xs)


-- definition of player

data Player = P1 | P2 deriving (Show, Eq)

-- who is the next player?

nextPlayer :: Player -> Player
nextPlayer P1 = P2
nextPlayer P2 = P1

-- each board entry is filled with blank (B)
-- an X or O

data Symbol = X | O | B deriving (Show, Eq)

-- which symbol a given player uses

symbolPlayer :: Player -> Symbol
symbolPlayer P1 = X
symbolPlayer P2 = O

-- definition of a board line

data Line = Line Symbol Symbol Symbol
            deriving Show

-- definition of the board

data Board = Board Line Line Line
             deriving Show

-- definition of columns and diagonals

data Column = Column Symbol Symbol Symbol deriving Show
data Diagonal = Diagonal Symbol Symbol Symbol deriving Show

-- function to return the columns

columns :: Board -> (Column, Column, Column)
columns (Board (Line s11 s12 s13)
               (Line s21 s22 s23)
               (Line s31 s32 s33)) = ( Column s11 s21 s31
                                     , Column s12 s22 s32
                                     , Column s13 s23 s33
                                     )
 
diagonal :: Board -> (Diagonal, Diagonal)
diagonal (Board (Line s11 _ s13)
                (Line _ s22 _)
                (Line s31 _ s33)) = ( Diagonal s11 s22 s33
                                    , Diagonal s13 s22 s31
                                    )

-- building the empty board

emptyBoard :: Board
emptyBoard = Board lineBlank lineBlank lineBlank
  where
    lineBlank = Line B B B

printSymbol :: Symbol -> String
printSymbol X = "X"
printSymbol O = "O"
printSymbol B = " "

printLine :: Line -> String
printLine (Line s1 s2 s3)
  = concat $ intersperse " | " $ map printSymbol [s1, s2, s3] 

bar :: String
bar = replicate 9 '-'

printBoard :: Board -> String
printBoard (Board l1 l2 l3)
  = unlines $ intersperse bar $ map printLine [l1, l2, l3] 

-- game trees

data Pos = One | Two | Three deriving Show

line :: Pos -> Board -> Line
line One (Board l1 _ _) = l1
line Two (Board _ l2 _) = l2
line Three (Board _ _ l3) = l3

column :: Pos -> Line -> Symbol
column One (Line s1 _ _) = s1
column Two (Line _ s2 _) = s2
column Three (Line _ _ s3) = s3

-- getting the list of moves by each line

movesByLine :: Player -> Line -> [Line]
movesByLine p (Line s1 s2 s3)
  = concat [ f One s1
           , f Two s2
           , f Three s3
           ]
    where
      x = symbolPlayer p
      f One B = [Line x s2 s3]
      f Two B = [Line s1 x s3]
      f Three B = [Line s1 s2 x]
      f _ _ = []

-- generating all possible moves in a board

moves :: Player -> Board -> [Board]
moves p (Board l1 l2 l3)
  = concat [ [Board x l2 l3 | x <- movesByLine p l1]
           , [Board l1 x l3 | x <- movesByLine p l2]
           , [Board l1 l2 x | x <- movesByLine p l3]
           ]

-- returns the player for an input symbol

playerSymbol :: Symbol -> Maybe Player
playerSymbol X = Just P1
playerSymbol O = Just P2
playerSymbol _ = Nothing

-- checks if a sequence of 3 symbols are from the same player

samePlayer :: (Symbol,Symbol,Symbol) -> Maybe Player
samePlayer (s1,s2,s3)
  | s1 == s2 && s2 == s3 = playerSymbol s1
  | otherwise = Nothing

-- function to combine Maybe results: If the first is a Just
-- we simply return the Just x value. If the first parameter
-- is Nothing, the result is the second one.

(.||.) :: Maybe a -> Maybe a -> Maybe a
(Just x) .||. _ = Just x
Nothing  .||. y = y

-- checking if a player wins by marking all positions of
-- one of the board lines.

winByLines :: Board -> Maybe Player
winByLines (Board (Line s11 s12 s13)
                  (Line s21 s22 s23)
                  (Line s31 s32 s33))
  = samePlayer (s11, s12, s13) .||.
    samePlayer (s21, s22, s23) .||.
    samePlayer (s31, s32, s33)

-- checking if a player wins by marking all positions of
-- one of the board columns

winByColumns :: Board -> Maybe Player
winByColumns (Board (Line s11 s12 s13)
                    (Line s21 s22 s23)
                    (Line s31 s32 s33))
  = samePlayer (s11, s21, s31) .||.
    samePlayer (s12, s22, s32) .||.
    samePlayer (s13, s23, s33)

-- checking if a player wins by marking all positions of
-- one of the board diagonals.

winByDiagonals :: Board -> Maybe Player
winByDiagonals (Board (Line s11 _ s13)
                      (Line _ s22 _)
                      (Line s31 _ s33))
  = samePlayer (s11, s22, s33) .||.
    samePlayer (s31, s22, s13)

-- The game has a winner only if some player wins by line
-- column or diagonal.

winner :: Board -> Maybe Player
winner b = winByLines b     .||.
           winByColumns b   .||.
           winByDiagonals b 

-- generating the complete game tree for a given board and
-- player. We stop the generation when we have a winner.

gameTree :: Player -> Board -> Rose Board
gameTree p b
  | isJust $ winner b = b :> []
  | otherwise = b :> map (gameTree p') (moves p b)
          where
            p' = nextPlayer p

-- minimax algorithm

minimax :: Player -> Rose Board -> Rose Int
minimax p = minimax' p p
  where
    maximum' [] = 0
    maximum' ((1 :> _) : _) = 1
    maximum' ((x :> _) : xs) = max x (maximum' xs)

    minimum' [] = 0
    minimum' (((-1) :> _) : _) = -1
    minimum' ((x :> _) : xs) = min x (minimum' xs)
    
    minimax' p1 p2 (b :> [])
      | winner b == (Just p1) = 1 :> []
      | winner b == (Just p2) = (-1) :> []
      | otherwise = 0 :> []

    minimax' p1 p2 (_ :> bs)
      | p1 == p2 = let ps = map (minimax' p1 (nextPlayer p1)) bs
                   in (maximum' ps) :> ps
      | otherwise = let ps = map (minimax' p1 (nextPlayer p2)) bs
                    in (minimum' ps) :> ps

-- calculating a move

makeMove :: Player -> Board -> Maybe Board
makeMove p b
  = case maximumBy' comp ts of
      Just r -> Just $ root (snd r)
      Nothing -> Nothing
    where
      gt = gameTree p b
      ps = minimax p gt
      ts = zip (children ps) (children gt)
      comp x y = compare (fst x) (fst y)
      maximumBy' _ [] = Nothing
      maximumBy' f xs = Just $ maximumBy f xs

-- checking if an input is a valid position

validInput :: String -> Bool
validInput s = s `elem` ["1", "2", "3"]

-- converting an string to position

parseInput :: String -> Pos
parseInput "1" = One
parseInput "2" = Two
parseInput "3" = Three
parseInput _   = error "Impossible! parseInput"

-- reading a position from keyboard

readInt :: String -> IO Pos
readInt label
  = do
       putStr ("Digite um valor para " ++ label)
       inp <- getLine
       if validInput inp then return $ parseInput inp
         else do
                putStrLn "Valor inválido!"
                readInt label

-- reading a board position

readPosition :: IO (Pos, Pos)
readPosition
  = (,) <$> readInt "linha:" <*> readInt "coluna:"

-- checking if a position is empty on the board

emptyPosition :: (Pos,Pos) -> Board -> Bool
emptyPosition (l,c) b
  = column c (line l b) == B

-- updating a board position

updateLine :: Pos -> Player -> Line -> Line
updateLine One p (Line _ s2 s3) = Line (symbolPlayer p) s2 s3
updateLine Two p (Line s1 _ s3) = Line s1 (symbolPlayer p) s3
updateLine Three p (Line s1 s2 _) = Line s1 s2 (symbolPlayer p)

updateBoard :: (Pos,Pos) -> Player -> Board -> Board
updateBoard (One, c) p (Board l1 l2 l3)
  = Board (updateLine c p l1) l2 l3
updateBoard (Two, c) p (Board l1 l2 l3)
  = Board l1 (updateLine c p l2) l3
updateBoard (Three, c) p (Board l1 l2 l3)
  = Board l1 l2 (updateLine c p l3)

-- function for a human player do its turn

playerTurn :: Board -> IO Board
playerTurn b
  = do
      p <- readPosition
      if not $ emptyPosition p b then
        putStrLn "Posição inválida!" >> playerTurn b
      else
        return $ updateBoard p P1 b

-- function for the computer do its turn

computerTurn :: Board -> IO Board
computerTurn b = case makeMove P2 b of
                   Just b1 -> return b1
                   Nothing -> return b

--  type to express the result of a turn

data Result = Draw
            | MoreTurns
            | Winner Player

-- checking if a board is draw

draw :: Board -> Bool
draw b
  = all (not . flip emptyPosition b) positions
    where
      positions = [(x,y) | x <- [One, Two, Three]
                         , y <- [One, Two, Three]]

-- checking if the game has ended or if it is possible to play more turns

result :: Board -> Result
result b
  | draw b = Draw
  | otherwise = case winner b of
                  Just x -> Winner x
                  Nothing -> MoreTurns

-- functions for printing the result

printWinner :: Player -> IO ()
printWinner P1 = putStrLn "O jogador 1 venceu!"
printWinner P2 = putStrLn "O computador venceu!"


printResult :: Result -> IO ()
printResult Draw = putStrLn "O jogo empatou!"
printResult (Winner x) = printWinner x
printResult _ = return ()

-- main game loop

loop :: Board -> IO ()
loop b
  = do
      putStrLn $ printBoard b
      case result b of
        MoreTurns -> do
          b1 <- playerTurn b
          case result b1 of
            MoreTurns -> do
              b2 <- computerTurn b1
              loop b2
            y -> do
              putStrLn $ printBoard b1
              printResult y
        x -> do
          putStrLn $ printBoard b
          printResult x

-- main function

main :: IO ()
main = loop emptyBoard
