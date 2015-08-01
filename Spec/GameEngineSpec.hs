{-# LANGUAGE OverloadedStrings #-}

module GameEngineSpec where


import           Game.Deterministic.GameEngine
import           MockGame
import           Test.Hspec


main :: IO ()
main = hspec spec


tieGame :: Monad m => MockGame m
tieGame = makeMockGame 0


oddWins :: Monad m => MockGame m
oddWins = makeMockGame 3


evenWins :: Monad m => MockGame m
evenWins = makeMockGame 4


spec :: Spec
spec = do
  describe "play" $
    it "should play the provided game within a specific context" $ do
      play tieGame  `shouldBe` [0]
      play oddWins  `shouldBe` [1]
      play evenWins `shouldBe` [-1]

  describe "playSimple" $
    it "should play the provided game without a context" $ do
      playSimple tieGame  `shouldBe`  0
      playSimple oddWins  `shouldBe`  1
      playSimple evenWins `shouldBe` -1

  describe "playIO" $
    it "should play the provided game within an IO context" $ do
      playIO tieGame  `shouldReturn`  0
      playIO oddWins  `shouldReturn`  1
      playIO evenWins `shouldReturn` -1
