module Day10 (solve1, solve2, parse) where

import Data.List (isInfixOf, find, sort)
import AOC.Utils (remove, middle)

data Line = Corrupted Char | Incomplete String deriving Show

parse :: String -> [Line]
parse = map (parseLine . removeLegals) . lines

solve1 :: [Line] -> Int
solve1 lns = sum [ corruptedScore c | Corrupted c <- lns ]

solve2 :: [Line] -> Int
solve2 lns = middle $ sort [ computeScore i | Incomplete i <- lns ]
    where computeScore = foldr (\c acc -> acc * 5  + incompleteScore c) 0

--------

legals, illegals :: [String]
legals = ["<>", "()", "{}", "[]"]
illegals = [[x,y] | x <- "([{<", y <- ")]}>", [x,y] `notElem` legals]

parseLine :: String -> Line
parseLine str =
    case find (`elem` illegals) (zipWith (\x y -> [x,y]) str (tail str)) of
        Just s -> Corrupted (s !! 1)
        Nothing -> Incomplete str

removeLegals :: String -> String
removeLegals str =
    if any (`isInfixOf` str) legals then
        removeLegals $ foldl (flip remove) str legals
    else
        str

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