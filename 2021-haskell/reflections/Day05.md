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
```

