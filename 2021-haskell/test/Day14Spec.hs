module Day14Spec (spec) where

import Day14 (solve1, solve2, parse)
import Test.Hspec

spec :: Spec
spec = do
    describe "" $ do

      let input = "NNCB\n\
                   \\n\
                   \CH -> B\n\
                   \HH -> N\n\
                   \CB -> H\n\
                   \NH -> C\n\
                   \HB -> C\n\
                   \HC -> B\n\
                   \HN -> C\n\
                   \NN -> C\n\
                   \BH -> H\n\
                   \NC -> B\n\
                   \NB -> B\n\
                   \BN -> B\n\
                   \BB -> N\n\
                   \BC -> B\n\
                   \CC -> N\n\
                   \CN -> C\n"

      it "Part 1" $ do
        Day14.solve1 (Day14.parse input) `shouldBe` 1588
    
      it "Part 2" $ do
        Day14.solve2 (Day14.parse input) `shouldBe` 2188189693529
