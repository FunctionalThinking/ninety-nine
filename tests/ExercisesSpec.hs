module ExercisesSpec (spec) where

import Test.Hspec

import Exercises

spec :: Spec
spec = do
  describe "myLast" $ do
    it "myLast" $ do
      myLast [1..10] `shouldBe` (10 :: Int)
  describe "myButLast" $ do
    it "myButLast" $ do
      myButLast [1..10] `shouldBe` (9 :: Int)
  describe "insertAt" $ do
    it "insertAt" $ do
      insertAt 'X' "abcde" 2 == "aXbcde"
  describe "range" $ do
    it "range" $ do
      range 1 10 `shouldBe` ([1..10] :: [Int])

