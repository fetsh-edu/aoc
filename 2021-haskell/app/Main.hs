module Main where

import Day01
import System.Environment
import qualified System.FilePath as FP
import Data.Function
import qualified Data.Map.Strict as Map
import Text.Printf (printf)
import Relude (readMaybe, Map)

newtype Day = Day Int
    deriving (Eq, Ord, Show)

data Part = One | Two
    deriving (Eq, Ord, Show)

data Puzzle = Puzzle Day Part
    deriving (Eq, Ord, Show)

puzzles :: Map Puzzle (String -> String)
puzzles =
  Map.fromList [ (Puzzle (Day 1) One, show . Day01.solve1 . Day01.parse)
               , (Puzzle (Day 1) Two, show . Day01.solve2 . Day01.parse)
               ]


main :: IO ()
main = do
    args <- getArgs
    let puzzle = args & argsToTuple >>= parseArgs
    input <-
        case puzzle of
            Left t -> fail t
            Right p -> p & fileNameFor & readFile
    let answer = doPuzzle puzzle input
    putStrLn answer

    where
        doPuzzle :: Either String Puzzle -> String -> String
        doPuzzle (Left p) _ = "No puzzle: " ++ p
        doPuzzle (Right puzzle) input =
            case Map.lookup puzzle puzzles of
                    Nothing -> "This day and part is not solved yet"
                    Just f -> f input

        fileNameFor :: Puzzle -> FilePath
        fileNameFor (Puzzle (Day d) _) =
            "inputs" FP.</> "day" <> printf "%02d" d <> ".txt"

        argsToTuple :: [String] -> Either String (String, String)
        argsToTuple (d : p : _) = Right (d, p)
        argsToTuple _ = Left "Wrong arguments. Expecting two: (day) and (part)"

        parseArgs :: (String, String) -> Either String Puzzle
        parseArgs (dayT, partT) =
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