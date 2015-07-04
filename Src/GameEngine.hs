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


module GameEngine (
    Symbol,
    GameState(..),
    Player(..),
    Move(..),
    GameActions(..),
    GameEngine(..),
    play,
    playSimple,
    playIO
  ) where


import Control.Monad.Identity
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


-- TODO: provided function should be forced to be an identity only function
-- forall a. a -> m a
play :: Monad m => (GameState a -> m (GameState a)) -> GameEngine a b -> m Int
-- ^ Run the provided game engine under a monadic context until a terminal state is reached.
-- Note: provided function should act as an identity only, and should not modify the game state.
play f engine
  | performWithState isTerminal engine = return . performWithState getScore engine $ performWithState getPlayer engine
  | otherwise = f (getNextState engine) >>= play f . GameEngine (actions engine)


playSimple :: GameEngine a b -> Int
-- ^ Run the provided game engine without a context until a terminal state is reached.
playSimple = runIdentity . play Identity


playIO :: (GameState a -> IO ()) -> GameEngine a b -> IO Int
-- ^ Run the provided game engine within an IO context until a terminal state is reached.
playIO f = play (\x -> f x >> return x)


getNextState :: GameEngine a b -> GameState a
getNextState engine = performWithState getResult engine $ performWithState getMove engine


performWithState :: (GameActions a b -> GameState a -> c) -> GameEngine a b -> c
performWithState f engine = f (actions engine) $ state engine
