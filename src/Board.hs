module Board
  ( initialBoard
  , showBoard
  ) where

import Types

-- | Size of the board (standard 8x8 checkers board).
boardSize :: Int
boardSize = 8

-- | Initialize the standard starting board.
initialBoard :: Board
initialBoard = [ initialRow r | r <- [0..boardSize-1] ]
  where
    initialRow r
      | r <= 2    = [ pieceFor Black r c | c <- [0..boardSize-1] ]
      | r >= 5    = [ pieceFor Red   r c | c <- [0..boardSize-1] ]
      | otherwise = replicate boardSize Empty
    pieceFor p r c
      | (r + c) `mod` 2 == 1 = Man p
      | otherwise            = Empty

-- | Pretty print the board.
showBoard :: Board -> String
showBoard b = unlines $ map showRow b
  where
    showRow = concatMap showPiece
    showPiece (Man Red)   = " r"
    showPiece (Man Black) = " b"
    showPiece (King Red)  = " R"
    showPiece (King Black)= " B"
    showPiece Empty       = " ."
