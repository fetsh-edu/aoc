module Day08 (solve1, solve2, parse) where

import AOC.Utils (splitOn, both, unDigits10)
import Data.List (sort, isSubsequenceOf)
import qualified Data.Map as M ((!), fromList)

parse :: String -> [([String], [String])]
parse = map (both (map sort . words) . splitOn " | ") . lines

solve1 :: [([String], [String])] -> Int
solve1 = length . filter (`elem` [1,4,7,8]) . concatMap solveLine

solve2 :: [([String], [String])] -> Int
solve2 = sum . map (unDigits10 . solveLine)

-----------------------------------

solveLine :: ([String], [String]) -> [Int]
solveLine (patterns, digits) = map(patternsMap M.!) digits
    where  
        d1 = head $ filter ((== 2) . length) patterns
        d7 = head $ filter ((== 3) . length) patterns
        d4 = head $ filter ((== 4) . length) patterns
        d8 = head $ filter ((== 7) . length) patterns
        d9 = head $ filter (\x -> length x == 6 && d4 `isSubsequenceOf` x) patterns
        d0 = head $ filter (\x -> length x == 6 && x /= d9 && d1 `isSubsequenceOf` x ) patterns
        d6 = head $ filter (\x -> length x == 6 && x /= d9 && x /= d0 ) patterns
        d3 = head $ filter (\x -> length x == 5 && d1 `isSubsequenceOf` x) patterns
        d5 = head $ filter (\x -> length x == 5 && x /= d3 && x `isSubsequenceOf` d9) patterns
        d2 = head $ filter (\x -> length x == 5 && x /= d3 && x /= d5 ) patterns
        patternsMap = M.fromList $ zip [d0, d1, d2, d3, d4, d5, d6, d7, d8, d9] [0 .. 9]