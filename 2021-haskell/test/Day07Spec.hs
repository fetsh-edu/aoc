module Day07Spec (spec) where

import Day07 (solve1, solve2, parse)
import Test.Hspec

spec :: Spec
spec = do
    describe "Day 6" $ do

      let input = "16,1,2,0,4,2,7,1,2,14"

      it "solves Day 7 Part 1" $ do
        Day07.solve1 (Day07.parse input) `shouldBe` 37

      it "solves Day 7 Part 2" $ do
        Day07.solve2 (Day07.parse input) `shouldBe` 168


