module AOC.Utils where

import Data.List (tails, sort, group, minimumBy, maximumBy)
import Data.Function (on)

range :: (Num a, Enum a) => a -> a -> [a]
range x y = [x, (x - signum (x - y))..y]

range' :: (Eq a, Num a, Enum a) => a -> a -> [a]
range' x y | x == y = [x..y]
           | otherwise = [x, (x - signum (x - y))..y]

slides :: Int -> [a] -> [[a]]
slides n = slidesWith n (:) []

slidesWith :: Int -> (a -> b -> b) -> b -> [a] -> [b]
slidesWith n f acc = foldr (zipWith f) (repeat acc) . take n . tails

bitsToDec :: (Foldable t, Num a) => t a -> a
bitsToDec = foldl (\acc x -> acc * 2 + x) 0

mostCommon :: (Ord a) => [a] -> a
mostCommon = frequent maximumBy

leastCommon :: (Ord a) => [a] -> a
leastCommon = frequent minimumBy

frequent :: (Ord a, Foldable t) => ((t a1 -> t a1 -> Ordering) -> [[a]] -> [c]) -> [a] -> c
frequent f = head . f (compare `on` length) . group . sort