module Day08 (solve1, solve2, parse) where

import AOC.Utils (splitOn, mapTuple, unDigits10)
import Data.List (sort, union, isSubsequenceOf, (\\))
import qualified Data.Map as M ((!), fromList)

parse :: String -> [([String], [String])]
parse = map (mapTuple (map sort . words) . splitOn " | ") . lines

solve1 :: [([String], [String])] -> Int
solve1 = length . filter (`elem` [1,4,7,8]) . concatMap solveLine

solve2 :: [([String], [String])] -> Int
solve2 = sum . map (unDigits10 . solveLine)

-----------------------------------

solveLine :: ([String], [String]) -> [Int]
solveLine (patterns, digits) = map(patternsMap M.!) digits
    where
        d1 = head $ filter (\x -> length x == 2) patterns
        d7 = head $ filter (\x -> length x == 3) patterns
        d4 = head $ filter (\x -> length x == 4) patterns
        d8 = head $ filter (\x -> length x == 7) patterns
        d9 = head $ filter (\x -> length x == 6 && length (x \\ union d4 d7) == 1) patterns
        d0 = head $ filter (\x -> length x == 6 && x /= d9 && d1 `isSubsequenceOf` x ) patterns
        d6 = head $ filter (\x -> length x == 6 && x /= d9 && x /= d0 ) patterns
        d3 = head $ filter (\x -> length x == 5 && d1 `isSubsequenceOf` x) patterns
        d5 = head $ filter (\x -> length x == 5 && x /= d3 && null (x \\ d9) ) patterns
        d2 = head $ filter (\x -> length x == 5 && x /= d3 && x /= d5 ) patterns
        patternsMap = M.fromList $ zip [d0, d1, d2, d3, d4, d5, d6, d7, d8, d9] [0 .. 9]