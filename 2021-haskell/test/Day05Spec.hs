module Day05Spec (spec) where

import Day05 (solve1, solve2, parse)
import Test.Hspec

spec :: Spec
spec = do
    describe "Day 5" $ do

      let input = "0,9 -> 5,9\n\
                  \8,0 -> 0,8\n\
                  \9,4 -> 3,4\n\
                  \2,2 -> 2,1\n\
                  \7,0 -> 7,4\n\
                  \6,4 -> 2,0\n\
                  \0,9 -> 2,9\n\
                  \3,4 -> 1,4\n\
                  \0,0 -> 8,8\n\
                  \5,5 -> 8,2\n"

      it "solves Day 5 Part 1" $ do
        Day05.solve1 (Day05.parse input) `shouldBe` 5

      it "solves Day 5 Part 2" $ do
        Day05.solve2 (Day05.parse input) `shouldBe` 12
