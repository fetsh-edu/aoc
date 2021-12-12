module Day11Spec (spec) where

import Day11 (solve1, solve2, parse)
import Test.Hspec

spec :: Spec
spec = do
    describe "" $ do
      let input = "5483143223\n\
                   \2745854711\n\
                   \5264556173\n\
                   \6141336146\n\
                   \6357385478\n\
                   \4167524645\n\
                   \2176841721\n\
                   \6882881134\n\
                   \4846848554\n\
                   \5283751526\n"

      it "Part 1" $ do
        Day11.solve1 (Day11.parse input) `shouldBe` 1656
    
      it "Part 2" $ do
        Day11.solve2 (Day11.parse input) `shouldBe` 195
