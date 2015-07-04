module GameEngine.GameActions (
    GameActions(..)
  ) where


import GameEngine.Player
import GameEngine.Move
import GameEngine.GameState


data GameActions a b = GameActions {
    getPlayer  :: GameState a -> Player,
    -- ^ Specifies which player has the move in the state

    getMove    :: GameState a -> Move b,
    -- ^ Returns a legal move in the state

    getResult  :: GameState a -> Move b -> GameState a,
    -- ^ The transition model, which determines the result of a move

    isTerminal :: GameState a -> Bool,
    -- ^ True if game is over, False otherwise

    getScore   :: GameState a -> Player -> Int
    -- ^ A utility function to determine the numeric value for a game that ends in a terminal state
  }
-- ^ Set of actions that defines how the game will be played
