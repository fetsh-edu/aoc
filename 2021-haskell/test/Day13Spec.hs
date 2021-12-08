module Day13Spec (spec) where

import Day13 (solve1, solve2, parse)
import Test.Hspec

spec :: Spec
spec = do
    describe "Day 6" $ do

      let input = "3,4,3,1,2"

      it "solves Day 6 Part 1" $ do
        Day13.solve1 (Day13.parse input) `shouldBe` 5934
    
      it "solves Day 6 Part 2" $ do
        Day13.solve2 (Day13.parse input) `shouldBe` 26984457539
