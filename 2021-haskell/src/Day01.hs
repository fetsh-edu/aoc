module Day01 (solve1, solve2, parse) where

parse :: String -> [Int]
parse = fmap read . lines

solve1 :: [Int] -> Int
solve1 = solve 1

solve2 :: [Int] -> Int
solve2 = solve 3

solve :: Int -> [Int] -> Int
solve int input =
    length . filter id $ zipWith (<) input (drop int input)