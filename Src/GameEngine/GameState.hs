module GameEngine.GameState (
    GameState(..)
  ) where


data GameState a = GameState a deriving (Eq, Show)
