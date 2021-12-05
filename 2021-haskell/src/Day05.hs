module Day05 where

import Data.List (group, sort)
import Data.List.Split

type Point = (Int, Int)
type Line = (Point, Point)

parse :: String -> [Line]
parse = map parseLine . lines
    where
        parseLine line = (head p, p !! 1) where p = map parsePoint $ splitOn " -> " line
        parsePoint point = (head p, p !! 1) where p = map read $ splitOn "," point

solve1 :: [Line] -> Int
solve1 = solve2 . filter (\((x1, y1), (x2, y2)) -> x1 == x2 || y1 == y2) 

solve2 :: [Line] -> Int
solve2 = length . filter (> 1) . map length . group . sort . concatMap linePoints

-------------------------------------------------

linePoints :: Line -> [Point]
linePoints ((x1, y1), (x2, y2)) = zip (range x1 x2) (range y1 y2)
    where
        range x y = [x, (x - signum (x - y))..y]
