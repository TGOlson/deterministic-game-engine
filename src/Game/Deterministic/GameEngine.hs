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
> playSimple game

-}

{-# LANGUAGE RankNTypes #-}


module Game.Deterministic.GameEngine (
    Symbol,
    GameState(..),
    Player(..),
    Move(..),
    GameActions(..),
    GameEngine(..),
    GameEngineSimple,
    GameEngineIO,
    play,
    playSimple,
    playIO
  ) where


import           Control.Monad.Identity
import           Game.Deterministic.GameEngine.GameActions
import           Game.Deterministic.GameEngine.GameState
import           Game.Deterministic.GameEngine.Move
import           Game.Deterministic.GameEngine.Player


data GameEngine m a b = GameEngine {
    actions :: GameActions m a b,
    -- ^ Defines how the game will be played

    state   :: GameState a
    -- ^ The current state of the game
  }
-- ^ Holds information about how the game is played, and the current state of the game.


type GameEngineSimple a b = GameEngine Identity a b
type GameEngineIO a b = GameEngine IO a b


play :: Monad m => GameEngine m a b -> m Int
-- ^ Run the provided game engine under a monadic context until a terminal state is reached.
play engine = do
  isTerm <- isTerminal gameActions gameState

  if isTerm then do
    player <- getPlayer gameActions gameState
    getScore gameActions gameState player
    else do
      nextState <- getNextState engine
      play $ GameEngine (actions engine) nextState
  where
    gameActions = actions engine
    gameState = state engine


playSimple :: GameEngineSimple a b -> Int
-- ^ Run the provided game engine without a context until a terminal state is reached.
playSimple = runIdentity . play


playIO :: GameEngineIO a b -> IO Int
-- ^ Run the provided game engine within an IO context until a terminal state is reached.
playIO = play


getNextState :: Monad m => GameEngine m a b -> m (GameState a)
getNextState engine = do
  nextMove <- getMove gameActions gameState
  getResult gameActions gameState nextMove
  where
    gameActions = actions engine
    gameState = state engine
