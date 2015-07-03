{-# LANGUAGE OverloadedStrings   #-}

module GameEngineSpec where


import Test.Hspec
import GameEngine
import MockGame


main :: IO ()
main = hspec spec



tieGame :: MockGame
tieGame = makeMockGame 0


oddWins :: MockGame
oddWins = makeMockGame 3


evenWins :: MockGame
evenWins = makeMockGame 4


spec :: Spec
spec =
  describe "play" $
    it "should play the provided game" $ do
      play tieGame  `shouldBe`  0
      play oddWins  `shouldBe`  1
      play evenWins `shouldBe` -1
