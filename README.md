# Deterministic Game Engine

-- S0: The initial state
-- Player(state): Specifies which player has the move in the state
-- Actions(state): Returns the set of legal moves in the state
-- Result(state, a): The transition model, which determines the result of a move
-- Terminal-test(s): true if game is over, false otherwise
-- Utility(s, p): A utility function to determine the numeric value for
--  a game that ends in a terminal state (state) for player (p)


```hs
play game
-- => 1
```
