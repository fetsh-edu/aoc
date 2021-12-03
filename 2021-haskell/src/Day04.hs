module Day04 (solve1, solve2, parse) where

parse :: String -> [String]
parse = lines 

solve1 :: [String] -> Int
solve1 = length

solve2 :: [String] -> Int
solve2 = sum . map (length . words)