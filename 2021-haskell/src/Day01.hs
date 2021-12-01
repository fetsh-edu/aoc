module Day01 (solve1, solve2) where

import Relude ((&))

solve1 :: String -> String
solve1 = solve 1

solve2 :: String -> String
solve2 = solve 3

solve :: Int -> String -> String
solve int input =
    zipWith (<) ii (drop int ii) & filter id & length & show
    where
        ii = input & lines & map read :: [Int]