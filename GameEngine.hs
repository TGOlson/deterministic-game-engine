module GameEngine (
    Symbol,
    GameState(..),
    Player(..),
    Move(..),
    GameActions(..),
    GameEngine(..),
    play
  ) where


type Symbol = Char


data GameState a = GameState a


data Player = Player Char


data Move a = Move a


data GameActions a b = GameActions {
    getPlayer  :: GameState a -> Player,
    getMoves   :: GameState a -> [Move b],
    getResult  :: GameState a -> Move b -> GameState a,
    isTerminal :: GameState a -> Bool,
    getScore   :: GameState a -> Player -> Int
  }


data GameEngine a b = GameEngine {
    gameActions :: GameActions a b,
    state       :: GameState a
  }


play :: GameEngine a b -> Int
play engine
  | performWithState isTerminal engine = performWithState getScore engine $ performWithState getPlayer engine
  | otherwise = play $ GameEngine (gameActions engine) (getNextState engine)


getNextState :: GameEngine a b -> GameState a
getNextState engine = performWithState getResult engine . head $ performWithState getMoves engine


performWithState :: (GameActions a b -> GameState a -> c) -> GameEngine a b -> c
performWithState f engine = f (gameActions engine) $ state engine
