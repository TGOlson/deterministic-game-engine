module GameEngine.GameActions (
    GameActions(..)
  ) where


import GameEngine.Player
import GameEngine.Move
import GameEngine.GameState


data GameActions a b = GameActions {
    getPlayer  :: GameState a -> Player,
    getMoves   :: GameState a -> [Move b],
    getResult  :: GameState a -> Move b -> GameState a,
    isTerminal :: GameState a -> Bool,
    getScore   :: GameState a -> Player -> Int
  }
