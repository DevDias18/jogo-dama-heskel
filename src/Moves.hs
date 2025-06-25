module Moves
  ( validMoves
  ) where

import Types
import Board (boardSize)

-- | Generate basic diagonal moves (without capturing).
-- Only forward moves for men are considered.
validMoves :: GameState -> [Move]
validMoves gs = concatMap pieceMoves positions
  where
    b = board gs
    positions = [ ((r,c), p)
                | r <- [0..boardSize-1]
                , c <- [0..boardSize-1]
                , let p = b !! r !! c
                , piecePlayer p == Just (currentPlayer gs)
                ]
    pieceMoves ((r,c), Man Red)   = simpleMoves [(r-1,c-1),(r-1,c+1)]
    pieceMoves ((r,c), Man Black) = simpleMoves [(r+1,c-1),(r+1,c+1)]
    pieceMoves ((r,c), King _)    = simpleMoves [(r-1,c-1),(r-1,c+1),(r+1,c-1),(r+1,c+1)]
    pieceMoves _ = []
    simpleMoves coords = [ Move (r,c) coord
                         | coord@(nr,nc) <- coords
                         , inBounds coord
                         , b !! nr !! nc == Empty
                         ]
    inBounds (x,y) = x >= 0 && x < boardSize && y >=0 && y < boardSize

piecePlayer :: Piece -> Maybe Player
piecePlayer (Man p)  = Just p
piecePlayer (King p) = Just p
piecePlayer _        = Nothing
