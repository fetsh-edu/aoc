module Day01Spec (spec) where

import qualified Day01 (solve1, solve2)
import Test.Hspec

spec :: Spec
spec = do
    describe "Day 1" $ do

      let input = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]

      it "solves Day 1 Part 1" $ do
        Day01.solve1 input `shouldBe` 7

      it "solves Day 1 Part 2" $ do
        Day01.solve2 input `shouldBe` 5
