module Day07 where
import Data.List (sort)
import Data.List.Split (splitOn)
import Data.Function
import AOC.Utils (triangular)

parse :: String -> [Int]
parse = map read . splitOn ","

solve1 :: [Int] -> Int
solve1 ints = sum $ map (fuelForPos median) ints
    where median = sort ints !! (length ints `div` 2)
          fuelForPos p1 p2 = abs(p1 - p2)

solve2 :: [Int] -> Int
solve2 = solve (\p1 p2 -> triangular(abs(p1 - p2)))

-----

solve :: (Int -> Int -> Int) -> [Int] -> Int
solve f poss =
    minimum ([0..(maximum poss)] & map (\p -> sum $ map (f p) poss))