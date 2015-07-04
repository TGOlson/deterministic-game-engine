module GameEngine (
    Symbol,
    GameState(..),
    Player(..),
    Move(..),
    GameActions(..),
    GameEngine(..),
    play
  ) where


import GameEngine.GameState
import GameEngine.GameActions
import GameEngine.Move
import GameEngine.Player


data GameEngine a b = GameEngine {
    actions :: GameActions a b,
    state   :: GameState a
  }


play :: GameEngine a b -> Int
play engine
  | performWithState isTerminal engine = performWithState getScore engine $ performWithState getPlayer engine
  | otherwise = play $ GameEngine (actions engine) (getNextState engine)


getNextState :: GameEngine a b -> GameState a
getNextState engine = performWithState getResult engine . head $ performWithState getMoves engine


performWithState :: (GameActions a b -> GameState a -> c) -> GameEngine a b -> c
performWithState f engine = f (actions engine) $ state engine
