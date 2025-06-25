module Main where

import Board
import GameLogic
import Types

-- | Run a simple machine vs machine game for the given number of turns.
playGame :: Int -> GameState -> IO ()
playGame 0 gs = putStrLn $ showBoard (board gs)
playGame n gs = do
  putStrLn $ "Turn: " ++ show (turns - n)
  putStrLn $ showBoard (board gs)
  step <- randomStep gs
  case step of
    Nothing   -> putStrLn "Game over"
    Just gs' -> playGame (n-1) gs'
  where
    turns = n

main :: IO ()
main = do
  putStrLn "Machine vs Machine demo"
  playGame 20 initialState
