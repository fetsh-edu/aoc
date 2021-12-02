module Day02Spec (spec) where

import Day02 (solve1, solve2, parse)
import Test.Hspec

spec :: Spec
spec = do
    describe "Day 2" $ do

      let input = "forward 5\n\
                  \down 5\n\
                  \forward 8\n\
                  \up 3\n\
                  \down 8\n\
                  \forward 2"

      it "solves Day 2 Part 1" $ do
        Day02.solve1 (Day02.parse input) `shouldBe` 150

      it "solves Day 2 Part 2" $ do
        Day02.solve2 (Day02.parse input) `shouldBe` 900
