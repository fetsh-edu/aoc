module AOC.Puzzles
    (Puzzle(..), Day(..), Part(..)
    , argsToPuzzle
    , argsToTuple
    , doPuzzle
    , fileNameFor
    , getPuzzleUnsafe
    , parseArgsTuple
    , puzzles
    ) where

import qualified Data.Map.Strict as MS
import qualified System.FilePath as FP
import Relude (readMaybe)
import Text.Printf (printf)
import Day01
import Day02
import Day03
import Day04
import Day05
import Day06
import Day07
import Day08
import Day09
import Day10
import Day11
import Day12
import Day13
import Day14
import Day15
import Day16
import Day17
import Day18
import Day19
import Day20
import Day21
import Day22
import Day23
import Day24
import Day25

newtype Day = Day Int
    deriving (Eq, Ord, Show)

data Part = One | Two
    deriving (Eq, Ord, Show)

data Puzzle = Puzzle Day Part
    deriving (Eq, Ord, Show)

fileNameFor :: Puzzle -> FilePath
fileNameFor (Puzzle (Day d) _) =
   "inputs" FP.</> "day" <> printf "%02d" d <> ".txt"

argsToPuzzle :: [String] -> Either String Puzzle
argsToPuzzle args =
    argsToTuple args >>= parseArgsTuple

argsToTuple :: [String] -> Either String (String, String)
argsToTuple (d : p : _) = Right (d, p)
argsToTuple _ = Left "Wrong arguments. Expecting two: (day) and (part)"

parseArgsTuple :: (String, String) -> Either String Puzzle
parseArgsTuple (dayT, partT) =
    Puzzle <$> parseDay dayT <*> parsePart partT
    where
        parseDay :: String -> Either String Day
        parseDay d =
            case readMaybe d of
                Nothing -> Left "Day should be a number"
                Just day -> if day <= 25 && day > 0
                            then Right (Day day)
                            else Left "'Day' must be between 1 and 25"
        parsePart :: String -> Either String Part
        parsePart p =
            case readMaybe p :: Maybe Int of
              Nothing -> Left "Part should be a number"
              Just part -> case part of
                           1 -> Right One
                           2 -> Right Two
                           _ -> Left "Part should be of 1 or 2"

getPuzzleUnsafe :: Puzzle -> (String -> String)
getPuzzleUnsafe puzzle = func
    where
        func = case MS.lookup puzzle puzzles of
            Nothing -> error "Puzzle iis not implemented"
            Just f -> f

doPuzzle :: String -> Puzzle -> String
doPuzzle input puzzle =
    case MS.lookup puzzle puzzles of
            Nothing -> "This day and part is not solved yet"
            Just f -> f input

puzzles :: MS.Map Puzzle (String -> String)
puzzles =
  MS.fromList [ (Puzzle (Day 1) One, show . Day01.solve1 . Day01.parse)
              , (Puzzle (Day 1) Two, show . Day01.solve2 . Day01.parse)
              , (Puzzle (Day 2) One, show . Day02.solve1 . Day02.parse)
              , (Puzzle (Day 2) Two, show . Day02.solve2 . Day02.parse)
              , (Puzzle (Day 3) One, show . Day03.solve1 . Day03.parse)
              , (Puzzle (Day 3) Two, show . Day03.solve2 . Day03.parse)
              , (Puzzle (Day 4) One, show . Day04.solve1 . Day04.parse)
              , (Puzzle (Day 4) Two, show . Day04.solve2 . Day04.parse)
              , (Puzzle (Day 5) One, show . Day05.solve1 . Day05.parse)
              , (Puzzle (Day 5) Two, show . Day05.solve2 . Day05.parse)
              , (Puzzle (Day 6) One, show . Day06.solve1 . Day06.parse)
              , (Puzzle (Day 6) Two, show . Day06.solve2 . Day06.parse)
              , (Puzzle (Day 7) One, show . Day07.solve1 . Day07.parse)
              , (Puzzle (Day 7) Two, show . Day07.solve2 . Day07.parse)
              , (Puzzle (Day 8) One, show . Day08.solve1 . Day08.parse)
              , (Puzzle (Day 8) Two, show . Day08.solve2 . Day08.parse)
              , (Puzzle (Day 9) One, show . Day09.solve1 . Day09.parse)
              , (Puzzle (Day 9) Two, show . Day09.solve2 . Day09.parse)
              , (Puzzle (Day 10) One, show . Day10.solve1 . Day10.parse)
              , (Puzzle (Day 10) Two, show . Day10.solve2 . Day10.parse)
              , (Puzzle (Day 11) One, show . Day11.solve1 . Day11.parse)
              , (Puzzle (Day 11) Two, show . Day11.solve2 . Day11.parse)
              , (Puzzle (Day 12) One, show . Day12.solve1 . Day12.parse)
              , (Puzzle (Day 12) Two, show . Day12.solve2 . Day12.parse)
              , (Puzzle (Day 13) One, show . Day13.solve1 . Day13.parse)
              , (Puzzle (Day 13) Two, show . Day13.solve2 . Day13.parse)
              , (Puzzle (Day 14) One, show . Day14.solve1 . Day14.parse)
              , (Puzzle (Day 14) Two, show . Day14.solve2 . Day14.parse)
              , (Puzzle (Day 15) One, show . Day15.solve1 . Day15.parse)
              , (Puzzle (Day 15) Two, show . Day15.solve2 . Day15.parse)
              , (Puzzle (Day 16) One, show . Day16.solve1 . Day16.parse)
              , (Puzzle (Day 16) Two, show . Day16.solve2 . Day16.parse)
              , (Puzzle (Day 17) One, show . Day17.solve1 . Day17.parse)
              , (Puzzle (Day 17) Two, show . Day17.solve2 . Day17.parse)
              , (Puzzle (Day 18) One, show . Day18.solve1 . Day18.parse)
              , (Puzzle (Day 18) Two, show . Day18.solve2 . Day18.parse)
              , (Puzzle (Day 19) One, show . Day19.solve1 . Day19.parse)
              , (Puzzle (Day 19) Two, show . Day19.solve2 . Day19.parse)
              , (Puzzle (Day 20) One, show . Day20.solve1 . Day20.parse)
              , (Puzzle (Day 20) Two, show . Day20.solve2 . Day20.parse)
              , (Puzzle (Day 21) One, show . Day21.solve1 . Day21.parse)
              , (Puzzle (Day 21) Two, show . Day21.solve2 . Day21.parse)
              , (Puzzle (Day 22) One, show . Day22.solve1 . Day22.parse)
              , (Puzzle (Day 22) Two, show . Day22.solve2 . Day22.parse)
              , (Puzzle (Day 23) One, show . Day23.solve1 . Day23.parse)
              , (Puzzle (Day 23) Two, show . Day23.solve2 . Day23.parse)
              , (Puzzle (Day 24) One, show . Day24.solve1 . Day24.parse)
              , (Puzzle (Day 24) Two, show . Day24.solve2 . Day24.parse)
              , (Puzzle (Day 25) One, show . Day25.solve1 . Day25.parse)
              , (Puzzle (Day 25) Two, show . Day25.solve2 . Day25.parse)
              ]