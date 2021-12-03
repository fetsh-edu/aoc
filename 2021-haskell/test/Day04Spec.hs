module Day04Spec (spec) where

import Day04 (solve1, solve2, parse)
import Test.Hspec

spec :: Spec
spec = do
    describe "Day 4" $ do

      let input = "qwery some"

      it "solves Day 4 Part 1" $ do
        Day04.solve1 (Day04.parse input) `shouldBe` 0

      it "solves Day 4 Part 2" $ do
        Day04.solve2 (Day04.parse input) `shouldBe` 0
