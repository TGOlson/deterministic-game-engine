module GameTest where

import GameEngine


isTerminalFn :: GameState [Symbol] -> Bool
isTerminalFn (GameState xs) = 'x' `elem` xs

-- hacky test game
myGameActions :: GameActions [Symbol] Int
myGameActions = GameActions {
    getPlayer  = const (Player 'x'),
    getMoves   = const [Move 2],
    getResult  = \_ _ -> GameState "axa",
    isTerminal = isTerminalFn,
    getScore   = const $ const 1
  }

game :: GameEngine [Symbol] Int
game = GameEngine {
    gameActions = myGameActions,
    state       = GameState "aaa"
  }
