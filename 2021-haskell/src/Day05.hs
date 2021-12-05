module Day05 where

import Data.List (group, sort)
import AOC.Utils (range, splitOn, mapTuple)

type Point = (Int, Int)
type Line = (Point, Point)

parse :: String -> [Line]
parse = map (mapTuple (mapTuple read . splitOn ",") . splitOn " -> ") . lines

solve1 :: [Line] -> Int
solve1 = solve2 . filter (\((x1, y1), (x2, y2)) -> x1 == x2 || y1 == y2) 

solve2 :: [Line] -> Int
solve2 = length . filter (> 1) . map length . group . sort
            . concatMap (\((x1, y1), (x2, y2)) -> zip (range x1 x2) (range y1 y2))