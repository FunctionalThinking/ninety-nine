module ExercisesSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck

import Exercises

spec :: Spec
spec = do
  describe "insertAt" $ do
    it "insertAt" $ do
      insertAt 'X' "abcde" 2 == "aXbcde"
  describe "range" $ do
    it "range" $ do
      range 1 10 == [1..10]

