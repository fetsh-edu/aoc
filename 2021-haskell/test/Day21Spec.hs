module Day21Spec (spec) where

import Day21 (solve1, solve2, parse)
import Test.Hspec

spec :: Spec
spec = do
    describe "Day 6" $ do

      let input = "3,4,3,1,2"

      it "solves Day 6 Part 1" $ do
        Day21.solve1 (Day21.parse input) `shouldBe` 5934
    
      it "solves Day 6 Part 2" $ do
        Day21.solve2 (Day21.parse input) `shouldBe` 26984457539
