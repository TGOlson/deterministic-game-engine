module GameActions where


import Player
import Move
import GameState


data GameActions a b = GameActions {
    getPlayer  :: GameState a -> Player,
    getMoves   :: GameState a -> [Move b],
    getResult  :: GameState a -> Move b -> GameState a,
    isTerminal :: GameState a -> Bool,
    getScore   :: GameState a -> Player -> Int
  }
