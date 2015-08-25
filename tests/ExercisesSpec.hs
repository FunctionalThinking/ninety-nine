module ExercisesSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck

import Exercises

spec :: Spec
spec = do
  describe "myLast" $ do
    it "myLast" $ do
      myLast [1..10] == 10
      myLast [1] == 1
  describe "myButLast" $ do
    it "myButLast" $ do
      myButLast [1..10] == 9
      myButLast [1..3] == 2
  describe "insertAt" $ do
    it "insertAt" $ do
      insertAt 'X' "abcde" 2 == "aXbcde"
  describe "range" $ do
    it "range" $ do
      range 1 10 == [1..10]

