module Day01 (solve1, solve2) where

import Relude ((&))

import Data.List (tails)

solve1 :: String -> String
solve1 input =
    input
        & ints
        & slides 2
        & filter (\x -> x !! 1 > head x) & length & show

solve2 :: String -> String
solve2 input =
    input
        & ints
        & slides 3
        & slides 2
        & filter (\x -> sum (x !! 1) > sum (head x)) & length & show

ints :: String -> [Int]
ints = map read . lines

slides :: Int -> [a] -> [[a]]
slides n =
     foldr (zipWith (:)) (repeat []) . take n . tails
