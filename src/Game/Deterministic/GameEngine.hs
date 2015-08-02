{- |

Library for creating simple deterministic games, such
as tic-tac-toe. The engine requires a minimal set of
actions related to the game, and then will run the game
until a terminal state is reached.

Simple generic example below. See the specs for a more detailed example.

> import Game.Deterministic.GameEngine
>
> game :: Monad m => GameEngine m [Char] [(Int, Char)]
> game = GameEngine gameActions initialState
>
> gameActions :: Monad m => GameActions m [Char] [(Int, Char)]
> gameActions = GameActions {
>    getPlayer  = -- find the next player from a game state,
>    getMove    = -- find a move from the game state,
>    getResult  = -- transitions from a state to another state,
>    isTerminal = -- determines if the game is terminal,
>    getScore   = -- get score from a terminal state
>  }
>
> initialState :: GameState [Char]
> initialState = GameState ['x', 'x', 'x']
>
> -- run the game engine until a terminal state is reached
> playSimple game

-}


module Game.Deterministic.GameEngine (
    GameActions(..),
    GameState(..),
    Move(..),
    Player(..),
    GameEngine(..),
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
    gameEngineActions :: GameActions m a b,
    -- ^ Defines how the game will be played

    gameEngineState   :: GameState a
    -- ^ The current state of the game
  }
-- ^ Holds information about how the game is played, and the current state of the game.


play :: Monad m => GameEngine m a b -> m Int
-- ^ Run the provided game engine under a monadic context until a terminal state is reached.
play (GameEngine actions state) = do
  isTerm <- isTerminal actions state

  if isTerm then do
    player <- getPlayer actions state
    getScore actions state player
    else do
      nextState <- getNextState actions state
      play $ GameEngine actions nextState


playSimple :: GameEngine Identity a b -> Int
-- ^ Run the provided game engine without a context until a terminal state is reached.
playSimple = runIdentity . play


playIO :: GameEngine IO a b -> IO Int
-- ^ Run the provided game engine within an IO context until a terminal state is reached.
playIO = play


getNextState :: Monad m => GameActions m a b -> GameState a -> m (GameState a)
getNextState actions state = do
  nextMove <- getMove actions state
  getResult actions state nextMove
