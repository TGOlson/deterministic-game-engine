module Game.Deterministic.GameEngine.Move (
    Move(..)
  ) where


data Move a = Move a deriving (Eq, Show)
