module Day09Spec (spec) where

import Day09 (solve1, solve2, parse)
import Test.Hspec

spec :: Spec
spec = do
    describe "Solves" $ do

      let input = "2199943210\n\
                   \3987894921\n\
                   \9856789892\n\
                   \8767896789\n\
                   \9899965678\n"

      it "Part 1" $ do
        Day09.solve1 (Day09.parse input) `shouldBe` 15
    
      it "Part 2" $ do
        Day09.solve2 (Day09.parse input) `shouldBe` 1134
