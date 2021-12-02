module Day02Spec (spec) where

import Day02 (solve1, solve2, Step(..))
import Test.Hspec

spec :: Spec
spec = do
    describe "Day 2" $ do

      let input = [Forward 5, Down 5, Forward 8, Up 3, Down 8, Forward 2]

      it "solves Day 2 Part 1" $ do
        Day02.solve1 input `shouldBe` 150

      it "solves Day 2 Part 2" $ do
        Day02.solve2 input `shouldBe` 900
