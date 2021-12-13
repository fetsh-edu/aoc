module Day13Spec (spec) where

import Day13 (solve1, solve2, parse)
import Test.Hspec

spec :: Spec
spec = do
    describe "" $ do

      let input = "6,10\n\
                   \0,14\n\
                   \9,10\n\
                   \0,3\n\
                   \10,4\n\
                   \4,11\n\
                   \6,0\n\
                   \6,12\n\
                   \4,1\n\
                   \0,13\n\
                   \10,12\n\
                   \3,4\n\
                   \3,0\n\
                   \8,4\n\
                   \1,10\n\
                   \2,14\n\
                   \8,10\n\
                   \9,0\n\
                   \\n\
                   \fold along y=7\n\
                   \fold along x=5\n"

      it "Part 1" $ do
        Day13.solve1 (Day13.parse input) `shouldBe` 17
    
--      it "Part 2" $ do
--        Day13.solve2 (Day13.parse input) `shouldBe` 26984457539
