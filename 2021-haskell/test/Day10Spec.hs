module Day10Spec (spec) where

import Day10 (solve1, solve2, parse)
import Test.Hspec

spec :: Spec
spec = do
    describe "" $ do

      let input = "[({(<(())[]>[[{[]{<()<>>\n\
                   \[(()[<>])]({[<{<<[]>>(\n\
                   \{([(<{}[<>[]}>{[]{[(<()>\n\
                   \(((({<>}<{<{<>}{[]{[]{}\n\
                   \[[<[([]))<([[{}[[()]]]\n\
                   \[{[{({}]{}}([{[{{{}}([]\n\
                   \{<[[]]>}<{[{[{[]{()[[[]\n\
                   \[<(<(<(<{}))><([]([]()\n\
                   \<{([([[(<>()){}]>(<<{{\n\
                   \<{([{{}}[<[[[<>{}]]]>[]]\n"

      it "Part 1" $ do
        Day10.solve1 (Day10.parse input) `shouldBe` 26397
    
      it "Part 2" $ do
        Day10.solve2 (Day10.parse input) `shouldBe` 288957
