# AoC 2021 Day 5

## Код, который решил задачу

```haskell
import Data.List (group, sort)
import Data.List.Split

type Point = (Int, Int)
type Line = (Point, Point)

parse :: String -> [Line]
parse = map parseLine . lines
    where
        parseLine line = (p1, p2)
            where [p1, p2] = map parsePoint $ splitOn " -> " line
        parsePoint point = (x,y)
            where [x,y] = map read $ splitOn "," point

solve1 :: [Line] -> Int
solve1 = result . concatMap noDiagLinePoint

solve2 :: [Line] -> Int
solve2 = result . concatMap linePoints

-------------------------------------------------

noDiagLinePoint :: Line -> [Point]
noDiagLinePoint p@((x1, y1), (x2, y2)) | x1 == x2 || y1 == y2 = linePoints p
                                       | otherwise = []

linePoints :: Line -> [Point]
linePoints ((x1, y1), (x2, y2)) = zip (range x1 x2) (range y1 y2)
    where
        range x y = [x, (x - signum (x - y))..y]

result :: [Point] -> Int
result =
    length . filter (\x -> length x > 1) . group . sort
```

## Рефактор

Такой себе :) Просто пошафлил код.

```haskell
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
```

## Бенчмарк
```
benchmarking Day 5/Puzzle (Day 5) One
time                 77.17 ms   (74.84 ms .. 78.81 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 80.08 ms   (78.67 ms .. 83.75 ms)
std dev              3.552 ms   (1.170 ms .. 5.882 ms)
                 
benchmarking Day 5/Puzzle (Day 5) Two
time                 143.9 ms   (141.1 ms .. 147.2 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 144.0 ms   (142.7 ms .. 145.8 ms)
std dev              2.365 ms   (1.151 ms .. 3.723 ms)
```