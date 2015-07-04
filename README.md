# Deterministic Game Engine

[![Build Status][travis-image]][travis-url]

[Available on Hackage](https://hackage.haskell.org/package/deterministic-game-engine)

Haskell library for creating simple deterministic games, such as tic-tac-toe. The engine requires a minimal set of actions related to the game, and then will run the game until a terminal state is reached.

### Required action set when defining a game engine:

* S0: The initial state (_s_)
* Player(_s_): Specifies which player has the move in the state
* Actions(_s_): Returns the set of legal moves in the state (_[a]_)
* Result(_s_, _a_): The transition model, which determines the result of a move
* Terminal-test(_s_): true if game is over, false otherwise
* Utility(_s_, _p_): A utility function to determine the numeric value for a game that ends in a terminal state (_s_) for player (_p_).

### Example

See the [MockGame](https://github.com/TGOlson/deterministic-game-engine/blob/master/Spec/MockGame.hs) and associated [spec](https://github.com/TGOlson/deterministic-game-engine/blob/master/Spec/GameEngineSpec.hs) for an example on how to build a simple game using the game engine.


Install dependencies

```
$ cabal install
```

Run tests

```
$ cabal test
```

### TODO
* Add haddock documentation

[travis-image]: https://travis-ci.org/TGOlson/deterministic-game-engine.svg?branch=master
[travis-url]: https://travis-ci.org/TGOlson/deterministic-game-engine
