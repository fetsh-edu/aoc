module Day07 where
import Data.List (sort)
import Data.List.Split (splitOn)
import AOC.Utils (triangular)

parse :: String -> [Int]
parse = map read . splitOn ","

solve1 :: [Int] -> Int
solve1 ints = sum $ map (fuelForPos median) ints
    where median = sort ints !! (length ints `div` 2)
          fuelForPos p1 p2 = abs(p1 - p2)

solve2 :: [Int] -> Int
solve2 ints = minimum $ map (fuelSum ints) [0..(maximum ints)]
    where fuelSum poss pos = sum $ map (fuelForPos pos) poss
          fuelForPos p1 p2 = triangular(abs(p1 - p2))