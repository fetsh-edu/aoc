module Day01 (solve1, solve2) where

import Relude ((&))

import Control.Applicative
import Data.List (tails)

solve1 :: String -> String
solve1 input =
    zipWith (-) (tail nums) nums & filter (> 0) & length & show
    where
        nums = ints input

solve2 :: String -> String
solve2 input =
    zipWith (-) (tail nums) nums & filter (> 0) & length & show
        where
            nums = zip3 (tail $ tail measurements) (tail measurements) measurements & map (\(a,b,c) -> a + b + c)
            measurements = ints input
            
ints :: String -> [Int]
ints = map read . lines

windows :: Int -> [a] -> [[a]]
windows m = getZipList . traverse ZipList . take m . tails
