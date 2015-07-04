module Player where


type Symbol = Char


data Player = Player Symbol deriving (Eq, Show)
