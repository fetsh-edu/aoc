module Day10 (solve1, solve2, parse) where

import Data.List (isInfixOf, find, sort)
import AOC.Utils (middle, removeAll, lookup')

data Line = Corrupted Char | Incomplete String deriving Show

open, close :: [Char]
open = "([{<"
close = ")]}>"

parse :: String -> [Line]
parse = map (parseLine . compact) . lines

solve1 :: [Line] -> Int
solve1 lns = sum [ lookup' c scores | Corrupted c <- lns ]
    where scores = zip close [3,57,1197,25137]

solve2 :: [Line] -> Int
solve2 lns = middle $ sort [ computeScore i | Incomplete i <- lns ]
    where computeScore = foldr (\c acc -> acc * 5  + lookup' c scores) 0
          scores = zip open [1,2,3,4]

--------

parseLine :: String -> Line
parseLine line =
    case find (`elem` close) line of
        Just s -> Corrupted s
        Nothing -> Incomplete line

compact :: String -> String
compact str
    | any (`isInfixOf` str) legals = compact (str `removeAll` legals)
    | otherwise = str
    where legals = zipWith (\x y -> [x,y]) open close