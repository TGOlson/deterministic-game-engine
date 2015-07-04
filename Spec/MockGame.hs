-- exposes a mock game for testing
-- computers take turn adding 1 to a total
-- if end total is odd comp 1 wins, even comp 2 wins
-- possible results: 1 (comp 1 wins), -1 (comp 2 wins), 0 (tie)

module MockGame (
    MockGame,
    makeMockGame
  ) where


import GameEngine


type MockGame = GameEngine Int Int
type MockGameState = GameState Int


oddPlayer :: Player
oddPlayer = Player '1'


evenPlayer :: Player
evenPlayer = Player '2'


getNextPlayer :: MockGameState -> Player
getNextPlayer (GameState x) = if odd x then evenPlayer else oddPlayer


isTerminalFn :: Int -> MockGameState -> Bool
isTerminalFn maxMoves (GameState x) = x >= maxMoves


getGameScore :: MockGameState -> Player -> Int
getGameScore (GameState 0) _ = 0
getGameScore (GameState x) _ = if odd x then 1 else -1


initialGameState :: MockGameState
initialGameState = GameState 0


makeMockGame :: Int -> MockGame
makeMockGame numMoves = GameEngine (makeGameActions numMoves) initialGameState


makeGameActions :: Int -> GameActions Int Int
makeGameActions numMoves = GameActions {
    getPlayer  = getNextPlayer,

    -- only valid move in this game is adding 1
    getMoves   = const [Move 1],
    getResult  = \(GameState s) (Move x) -> GameState (s + x),
    isTerminal = isTerminalFn numMoves,
    getScore   = getGameScore
  }
