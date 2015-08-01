module Game.Deterministic.GameEngine.GameActions (
    GameActions(..)
  ) where


import           Game.Deterministic.GameEngine.GameState
import           Game.Deterministic.GameEngine.Move
import           Game.Deterministic.GameEngine.Player


data GameActions m a b = GameActions {
    getPlayer  :: GameState a -> m Player,
    -- ^ Specifies which player has the move in the state

    getMove    :: GameState a -> m (Move b),
    -- ^ Returns a legal move in the state

    getResult  :: GameState a -> Move b -> m (GameState a),
    -- ^ The transition model, which determines the result of a move

    isTerminal :: GameState a -> m Bool,
    -- ^ True if game is over, False otherwise

    getScore   :: GameState a -> Player -> m Int
    -- ^ A utility function to determine the numeric value for a game that ends in a terminal state
  }
-- ^ Set of actions that defines how the game will be played
