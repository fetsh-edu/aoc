module Day08 (solve1, solve2, parse) where

import AOC.Utils (splitOn, mapTuple)
import qualified Data.List as L (sort, union, isSubsequenceOf)
import Data.List ((\\))

parse :: String -> [([String], [String])]
parse = map (mapTuple (map L.sort . words) . splitOn " | ") . lines

solve1 :: [([String], [String])] -> Int
solve1 = length . filter (`elem` [1,4,7,8]) . concatMap parseLine

solve2 :: [([String], [String])] -> Int
solve2 = sum . map (read . concatMap show . parseLine)

-----------------------------------

parseLine :: ([String], [String]) -> [Int]
parseLine (patterns, digits) = nDigits
    where
        one = head $ filter (\x -> length x == 2) patterns
        seven = head $ filter (\x -> length x == 3) patterns
        four = head $ filter (\x -> length x == 4) patterns
        eight = head $ filter (\x -> length x == 7) patterns
        nine = head $ filter (\x -> length x == 6 && length (x \\ L.union four seven) == 1) patterns
        zero = head $ filter (\x -> length x == 6 && one `L.isSubsequenceOf` x && x /= nine ) patterns
        six = head $ filter (\x -> length x == 6 && x /= nine && x /= zero ) patterns
        three = head $ filter (\x -> length x == 5 && one `L.isSubsequenceOf` x) patterns
        five = head $ filter (\x -> length x == 5 && null (x \\ nine) && x /= three ) patterns
        two = head $ filter (\x -> length x == 5 && x /= three && x /= five ) patterns
        nDigits = map(\ds ->
                        if ds == one then  1
                        else if ds == seven then 7
                        else if ds == four then 4
                        else if ds == eight then 8
                        else if ds == nine then 9
                        else if ds == zero then 0
                        else if ds == six then 6
                        else if ds == three then 3
                        else if ds == five then 5
                        else if ds == two then 2
                        else error "AAAAAAAAA"
                        ) digits