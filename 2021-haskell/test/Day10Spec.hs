module Day10Spec (spec) where

import Day10 (solve1, solve2, parse)
import Test.Hspec

spec :: Spec
spec = do
    describe "" $ do

      let input = "3,4,3,1,2"

      it "Part 1" $ do
        Day10.solve1 (Day10.parse input) `shouldBe` 5934
    
      it "Part 2" $ do
        Day10.solve2 (Day10.parse input) `shouldBe` 26984457539
