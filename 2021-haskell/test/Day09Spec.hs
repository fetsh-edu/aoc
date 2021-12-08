module Day09Spec (spec) where

import Day09 (solve1, solve2, parse)
import Test.Hspec

spec :: Spec
spec = do
    describe "Solves" $ do

      let input = "3,4,3,1,2"

      it "Part 1" $ do
        Day09.solve1 (Day09.parse input) `shouldBe` 5934
    
      it "Part 2" $ do
        Day09.solve2 (Day09.parse input) `shouldBe` 26984457539
