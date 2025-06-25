module Types where

-- | Represents the two players of the game.
data Player = Red | Black deriving (Eq, Show)

-- | A piece on the board can be a man or a king of a specific player,
-- or the square can be empty.
data Piece = Man Player | King Player | Empty deriving (Eq, Show)

type Position = (Int, Int)

-- | Board is represented as rows of pieces.
type Board = [[Piece]]

-- | Represents a move from one position to another.
data Move = Move Position Position deriving (Eq, Show)

-- | Holds the current state of the game.
data GameState = GameState
  { board :: Board
  , currentPlayer :: Player
  } deriving (Show)
