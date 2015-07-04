{-# LANGUAGE OverloadedStrings #-}

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
spec = do
  describe "play" $
    it "should play the provided game within the provided context" $ do
      play (:[]) tieGame  `shouldBe` [0]
      play (:[]) oddWins  `shouldBe` [1]
      play (:[]) evenWins `shouldBe` [-1]

  describe "playSimple" $
    it "should play the provided game without a context" $ do
      playSimple tieGame  `shouldBe`  0
      playSimple oddWins  `shouldBe`  1
      playSimple evenWins `shouldBe` -1

  describe "playIO" $
    it "should play the provided game within an IO context" $ do
      playIO print tieGame  `shouldReturn`  0
      playIO print oddWins  `shouldReturn`  1
      playIO print evenWins `shouldReturn` -1
