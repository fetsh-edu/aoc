module Day20Spec (spec) where

import Day20 (solve1, solve2, parse)
import Test.Hspec

spec :: Spec
spec = do
    describe "Day 6" $ do

      let input = "3,4,3,1,2"

      it "solves Day 6 Part 1" $ do
        Day20.solve1 (Day20.parse input) `shouldBe` 5934
    
      it "solves Day 6 Part 2" $ do
        Day20.solve2 (Day20.parse input) `shouldBe` 26984457539
