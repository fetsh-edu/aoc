module Day12Spec (spec) where

import Day12 (solve1, solve2, parse)
import Test.Hspec

spec :: Spec
spec = do
    describe "" $ do

      let input = "dc-end\n\
                   \HN-start\n\
                   \start-kj\n\
                   \dc-start\n\
                   \dc-HN\n\
                   \LN-dc\n\
                   \HN-end\n\
                   \kj-sa\n\
                   \kj-HN\n\
                   \kj-dc\n"

      it "Part 1" $ do
        Day12.solve1 (Day12.parse input) `shouldBe` 19
    
--      it "Part 2" $ do
--        Day12.solve2 (Day12.parse input) `shouldBe` 26984457539
