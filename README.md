# Deterministic Game Engine

* S0: The initial state (_s_)
* Player(_s_): Specifies which player has the move in the state
* Actions(_s_): Returns the set of legal moves in the state (_[a]_)
* Result(_s_, _a_): The transition model, which determines the result of a move
* Terminal-test(_s_): true if game is over, false otherwise
* Utility(_s_, _p_): A utility function to determine the numeric value for a game that ends in a terminal state (_s_) for player (_p_).


Install dependencies

```
$ cabal install
```

Run tests

```
$ cabal run unit-tests
```

### TODO
* Add haddock documentation
