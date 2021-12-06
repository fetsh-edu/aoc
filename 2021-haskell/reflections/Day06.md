# AOC 2021 Day 6

## Код, который решил задачу

Начал с наивного брутфорса, создавая список рыбок, первая часть решилась. 
На второй части моих 32 ГБ памяти не хватило.

```haskell
data Fish = Fish Int Int deriving (Show)

solve1 :: [Fish] -> Int
solve1 = length . fishesAfter 80 . map (Fish 7)
---- or: sum . map (fishesAfter' 256)

fishesAfter :: Int -> [Fish] -> [Fish]
fishesAfter days fishes = foldl spawn fishes [1..days]

spawn :: [Fish] -> Int -> [Fish]
spawn fishes _ = map age fishes ++ replicate (length $ filter (\(Fish _ d) -> d == 0) fishes) (Fish 9 8)
    where age (Fish base days) = Fish base (mod (days - 1) (if days == 0 then 7 else base))
```

Попробовал переписать, но все равно считал каждую рыбку:

```haskell
solve1 :: [Int] -> Int
solve1 = sum . map (fishesAfter' 256)

fishesAfter' :: Int -> Int -> Int
fishesAfter' days age =
    1 + sum (map (\i -> fishesAfter' (days - i) 8) $ take children $ iterate (+7) (age + 1))
    where children = (7 - age + (days - 1))  `div` 7
```

Пришлось нарисовать табличку, чтобы понять, что происходит:
```
3,4,3,1,2
2,3,2,0,1
1,2,1,6,0,8
0,1,0,5,6,7,8
6,0,6,4,5,6,7,8,8
5,6,5,3,4,5,6,7,7,8
4,5,4,2,3,4,5,6,6,7
3,4,3,1,2,3,4,5,5,6
2,3,2,0,1,2,3,4,4,5
1,2,1,6,0,1,2,3,3,4,8

    0 > d1 d2 d3 d4 d5 d6 d7 d8 d9

0 = 0    1  1  2  1  0  0  0  1  1
1 = 1    1  2  1  0  0  0  1  1  3
2 = 1    2  1  0  0  0  1  1  3  2
3 = 2    1  0  0  0  1  1  3  2  2
4 = 1    0  0  0  1  1  3  2  2  1
5 = 0    0  0  1  1  3  2  2  1  0
6 = 0    0  1  1  3  2  2  1  0  1
7 = 0    0  0  1  1  2  1  0  0  0
8 = 0    0  1  1  2  1  0  0  0  1
```

В результате получилось такое:
```haskell
solve2 :: [Int] -> Int
solve2 = fishesAfter'' 256

fishesAfter'' :: Int -> [Int] -> Int
fishesAfter'' days fishes =
    let
        fishesCounter = M.fromList $ map (\x -> (head x, length x)) $ L.group $ L.sort fishes
        spawn' acc = M.fromList
            [ (0, M.findWithDefault 0 1 acc)
            , (1, M.findWithDefault 0 2 acc)
            , (2, M.findWithDefault 0 3 acc)
            , (3, M.findWithDefault 0 4 acc)
            , (4, M.findWithDefault 0 5 acc)
            , (5, M.findWithDefault 0 6 acc)
            , (6, M.findWithDefault 0 7 acc + M.findWithDefault 0 0 acc)
            , (7, M.findWithDefault 0 8 acc)
            , (8, M.findWithDefault 0 0 acc)
            ]
    in
      sum $ M.elems $ iterate spawn' fishesCounter !! days
```