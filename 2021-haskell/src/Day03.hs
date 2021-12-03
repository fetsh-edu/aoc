module Day03 (solve1, solve2, parse) where

import Data.List (transpose, maximumBy, minimumBy, group, sort, foldl')
import Data.Char (digitToInt)
import Data.Function (on)

parse :: String -> [[Int]]
parse = map (map digitToInt) . lines

solve1 :: [[Int]] -> Int
solve1 l = gamma * epsilon
    where
       gamma = toDec $ map mostCommon $ transpose l
       epsilon = toDec $ map leastCommon $ transpose l

solve2 :: [[Int]] -> Int
solve2 l = oxy * co2
    where
        oxy = toDec $ oxygen $ transpose l
        co2 = toDec $ co2gen $ transpose l

------------------------------------------------
toDec :: [Int] -> Int
toDec = foldl' (\acc x -> acc * 2 + x) 0

leastCommon :: [Int] -> Int
leastCommon = head . minimumBy (compare `on` length) . group . sort

mostCommon :: [Int] -> Int
mostCommon = head . maximumBy (compare `on` length) . group . sort

oxygen :: [[Int]] -> [Int]
oxygen [] = []
oxygen transposed@(x : _) = mC : oxygen filtered
    where
        mC = mostCommon x
        filtered = tail $ transpose $ filter (\num -> head num == mC) $ transpose transposed

co2gen :: [[Int]] -> [Int]
co2gen [] = []
co2gen transposed@(x : _) = lC : co2gen filtered
    where
        lC = leastCommon x
        filtered = tail $ transpose $ filter (\num -> head num == lC) $ transpose transposed