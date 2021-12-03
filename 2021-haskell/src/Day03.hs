module Day03 (solve1, solve2, parse) where

import Data.List (transpose, maximumBy, minimumBy, group, sort)
import Data.Char (digitToInt)
import Data.Function (on)

parse :: String -> [[Int]]
parse = map (map digitToInt) . lines

solve1 :: [[Int]] -> Int
solve1 l = bitsToDec mostCommons * bitsToDec (map flipBit mostCommons)
    where
       mostCommons = map (frequent maximumBy) $ transpose l

solve2 :: [[Int]] -> Int
solve2 l = bitsToDec oxy * bitsToDec co2
    where
        oxy = generator (frequent maximumBy) $ transpose l
        co2 = generator (frequent minimumBy) $ transpose l

------------------------------------------------

flipBit :: Int -> Int
flipBit i | i == 0 = 1
          | i == 1 = 0
          | otherwise = error (show i ++ " is not a bit")

bitsToDec :: (Foldable t, Num a) => t a -> a
bitsToDec = foldl (\acc x -> acc * 2 + x) 0

frequent :: (Ord a) => (([a] -> [a] -> Ordering) -> [[a]] -> [c]) -> [a] -> c
frequent f = head . f (compare `on` length) . group . sort

generator :: ([Int] -> Int) -> [[Int]] -> [Int]
generator _ [] = []
generator f transposed@(x : _) = lC : generator f filtered
    where
        lC = f x
        filtered = tail $ transpose $ filter (\num -> head num == lC) $ transpose transposed

