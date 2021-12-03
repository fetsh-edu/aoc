module Day03Spec (spec) where

import Day03 (solve1, solve2, parse)
import Test.Hspec

spec :: Spec
spec = do
    describe "Day 3" $ do

      let input = "00100\n\
                  \11110\n\
                  \10110\n\
                  \10111\n\
                  \10101\n\
                  \01111\n\
                  \00111\n\
                  \11100\n\
                  \10000\n\
                  \11001\n\
                  \00010\n\
                  \01010\n"

      it "solves Day 3 Part 1" $ do
        Day03.solve1 (Day03.parse input) `shouldBe` 198

      it "solves Day 3 Part 2" $ do
        Day03.solve2 (Day03.parse input) `shouldBe` 230
