{- |

Haskell library for creating simple deterministic games,
such as tic-tac-toe. The engine requires a minimal set of
actions related to the game, and then will run the game
until a terminal state is reached.

Simple generic example below. See the specs for a more detailed example.

> import GameEngine
>
> game :: GameEngine Int Int
> game = GameEngine gameActions initialState
>
> gameActions :: GameActions Int Int
> gameActions = GameActions {
>    getPlayer  = -- find the next player from a game state,
>    getMove    = -- find a move from the game state,
>    getResult  = -- transitions from a state to another state,
>    isTerminal = -- determines if the game is terminal,
>    getScore   = -- get score from a terminal state
>  }
>
> initialState :: GameState Int
> initialState = GameState 0
>
> -- run the game engine until a terminal state is reached
> play game

-}


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
    -- ^ Defines how the game will be played

    state   :: GameState a
    -- ^ The current state of the game
  }
-- ^ Holds information about how the game is played, and the current state of the game.


play :: GameEngine a b -> Int
-- ^ Run the provided game engine until a terminal state is reached.
play engine
  | performWithState isTerminal engine = performWithState getScore engine $ performWithState getPlayer engine
  | otherwise = play $ GameEngine (actions engine) (getNextState engine)


getNextState :: GameEngine a b -> GameState a
getNextState engine = performWithState getResult engine $ performWithState getMove engine


performWithState :: (GameActions a b -> GameState a -> c) -> GameEngine a b -> c
performWithState f engine = f (actions engine) $ state engine
