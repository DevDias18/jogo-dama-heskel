module GameLogic
  ( initialState
  , randomStep
  ) where

import System.Random (randomRIO)
import Types
import Board
import Moves

-- | Create the initial game state.
initialState :: GameState
initialState = GameState initialBoard Red

-- | Apply a move to the board.
applyMove :: Board -> Move -> Board
applyMove b (Move (r,c) (r',c')) = replace2 r' c' (b !! r !! c) $ replace2 r c Empty b
  where
    replace2 x y val rows = take x rows ++ [replace y val (rows !! x)] ++ drop (x+1) rows
    replace idx v row = take idx row ++ [v] ++ drop (idx+1) row

-- | Perform a random valid move for the current player.
-- Returns Nothing if there are no moves available.
randomStep :: GameState -> IO (Maybe GameState)
randomStep gs = case validMoves gs of
  []    -> return Nothing
  moves -> do
    idx <- randomRIO (0, length moves - 1)
    let mv = moves !! idx
        newBoard = applyMove (board gs) mv
        nextPlayer = if currentPlayer gs == Red then Black else Red
    return $ Just gs { board = newBoard, currentPlayer = nextPlayer }
