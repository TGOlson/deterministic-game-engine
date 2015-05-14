module GameEngine where

-- S0: The initial state
-- Player(state): Specifies which player has the move in the state
-- Actions(state): Returns the set of legal moves in the state
-- Result(state, a): The transition model, which determines the result of a move
-- Terminal-test(s): true if game is over, false otherwise
-- Utility(s, p): A utility function to determine the numeric value for
--  a game that ends in a terminal state (state) for player (p)

-- makeGame ::

data Game a = Game a

type Symbol = Char

data Player = Player Char

data Move a = Move a

data GameEngine a b = GameEngine {
    state        :: Game a,
    getPlayer    :: Game a -> Player,
    getActions   :: Game a -> [Move b],
    getResult    :: Game a -> Move b -> Game a,
    isTerminal   :: Game a -> Bool,
    getScore     :: Game a -> Int
  }

isTerminalFn :: Game [Symbol] -> Bool
isTerminalFn (Game xs) = 'x' `elem` xs

game :: GameEngine [Symbol] Int
game = GameEngine {
    state      = Game "aaa",
    getPlayer  = const (Player 'x'),
    getActions = const [Move 2],
    getResult  = (\_ _ -> Game "axa"),
    isTerminal = isTerminalFn,
    getScore   = const 1
  }

-- transitionToState :: Game a -> GameEngine a b -> GameEngine a b
-- transitionToState s GameEngine {state = _, getPlayer = getPlayer, getActions = getActions, getResult = getResult, isTerminal = isTerminal, getScore = getScore} =
--   GameEngine {state = s, getPlayer = getPlayer, getActions = getActions, getResult = getResult, isTerminal = isTerminal, getScore = getScore}


play :: GameEngine a b -> Int
play (GameEngine {state = s, getPlayer = getPlayer, getActions = getActions, getResult = getResult, isTerminal = isTerminal, getScore = getScore})
  | isTerminal s = getScore s
  | otherwise = play GameEngine {state = nextBoard, getPlayer = getPlayer, getActions = getActions, getResult = getResult, isTerminal = isTerminal, getScore = getScore}
      where nextBoard = getResult s . head $ getActions s
