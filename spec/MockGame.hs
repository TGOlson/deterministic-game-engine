-- exposes a mock game for testing
-- computers take turn adding 1 to a total
-- if end total is odd comp 1 wins, even comp 2 wins
-- possible results: 1 (comp 1 wins), -1 (comp 2 wins), 0 (tie)

module MockGame (
    MockGame,
    MockGameState,
    makeMockGame
  ) where


import           Game.Deterministic.GameEngine


type MockGame m = GameEngine m Int Int
type MockGameState = GameState Int


oddPlayer :: Player
oddPlayer = Player "1"


evenPlayer :: Player
evenPlayer = Player "2"


getNextPlayer :: Monad m => MockGameState -> m Player
getNextPlayer (GameState x) = return $ if odd x then evenPlayer else oddPlayer


isTerminalFn :: Monad m => Int -> MockGameState -> m Bool
isTerminalFn maxMoves (GameState x) = return $ x >= maxMoves


getGameScore :: Monad m => MockGameState -> Player -> m Int
getGameScore (GameState 0) _ = return 0
getGameScore (GameState x) _ = return $ if odd x then 1 else -1


initialGameState :: MockGameState
initialGameState = GameState 0


makeMockGame :: Monad m => Int -> MockGame m
makeMockGame numMoves = GameEngine (makeGameActions numMoves) initialGameState


makeGameActions :: Monad m => Int -> GameActions m Int Int
makeGameActions numMoves = GameActions {
    getPlayer  = getNextPlayer,

    -- only valid move in this game is adding 1
    getMove    = const . return $ Move 1,
    getResult  = \(GameState s) (Move x) -> return $ GameState (s + x),
    isTerminal = isTerminalFn numMoves,
    getScore   = getGameScore
  }
