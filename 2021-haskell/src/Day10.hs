module Day10 (solve1, solve2, parse) where

import Data.List (isInfixOf, find, sort)
import AOC.Utils (middle, removeAll)

data Line = Corrupted Char | Incomplete String deriving Show

parse :: String -> [Line]
parse = map (parseLine . compact) . lines

solve1 :: [Line] -> Int
solve1 lns = sum [ corruptedScore c | Corrupted c <- lns ]

solve2 :: [Line] -> Int
solve2 lns = middle $ sort [ computeScore i | Incomplete i <- lns ]
    where computeScore = foldr (\c acc -> acc * 5  + incompleteScore c) 0

--------

parseLine :: String -> Line
parseLine line =
    case find (`elem` [')', ']', '}', '>']) line of
        Just s -> Corrupted s
        Nothing -> Incomplete line

compact :: String -> String
compact str
    | any (`isInfixOf` str) legals = compact (removeAll legals str)
    | otherwise = str
    where legals = ["<>", "()", "{}", "[]"]

corruptedScore :: Char -> Int
corruptedScore ')' = 3
corruptedScore ']' = 57
corruptedScore '}' = 1197
corruptedScore '>' = 25137
corruptedScore  _  = 0

incompleteScore :: Char -> Int
incompleteScore '(' = 1
incompleteScore '[' = 2
incompleteScore '{' = 3
incompleteScore '<' = 4
incompleteScore  _  = 0