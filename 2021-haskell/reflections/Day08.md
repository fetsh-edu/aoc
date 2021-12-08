# AOC 2021 Day 8

## Код, который решил задачу
```haskell
parse :: String -> [([String], [String])]
parse = map (mapTuple (map L.sort . words) . splitOn " | ") . lines

solve1 :: [([String], [String])] -> Int
solve1 = length . filter (`elem` [1,4,7,8]) . concatMap parseLine

solve2 :: [([String], [String])] -> Int
solve2 = sum . map (read . concatMap show . parseLine)

-----------------------------------

parseLine :: ([String], [String]) -> [Int]
parseLine (patterns, digits) = nDigits
    where
        one = head $ filter (\x -> length x == 2) patterns
        seven = head $ filter (\x -> length x == 3) patterns
        four = head $ filter (\x -> length x == 4) patterns
        eight = head $ filter (\x -> length x == 7) patterns
        nine = head $ filter (\x -> length x == 6 && length (x \\ L.union four seven) == 1) patterns
        zero = head $ filter (\x -> length x == 6 && one `L.isSubsequenceOf` x && x /= nine ) patterns
        six = head $ filter (\x -> length x == 6 && x /= nine && x /= zero ) patterns
        three = head $ filter (\x -> length x == 5 && one `L.isSubsequenceOf` x) patterns
        five = head $ filter (\x -> length x == 5 && null (x \\ nine) && x /= three ) patterns
        two = head $ filter (\x -> length x == 5 && x /= three && x /= five ) patterns
        nDigits = map(\ds ->
                        if ds == one then  1
                        else if ds == seven then 7
                        else if ds == four then 4
                        else if ds == eight then 8
                        else if ds == nine then 9
                        else if ds == zero then 0
                        else if ds == six then 6
                        else if ds == three then 3
                        else if ds == five then 5
                        else if ds == two then 2
                        else error "AAAAAAAAA"
                        ) digits
```

Добавляем Map

```haskell
solveLine :: ([String], [String]) -> [Int]
solveLine (patterns, digits) = map(patternsMap M.!) digits
    where
        d1 = head $ filter (\x -> length x == 2) patterns
        d7 = head $ filter (\x -> length x == 3) patterns
        d4 = head $ filter (\x -> length x == 4) patterns
        d8 = head $ filter (\x -> length x == 7) patterns
        d9 = head $ filter (\x -> length x == 6 && length (x \\ union d4 d7) == 1) patterns
        d0 = head $ filter (\x -> length x == 6 && x /= d9 && d1 `isSubsequenceOf` x ) patterns
        d6 = head $ filter (\x -> length x == 6 && x /= d9 && x /= d0 ) patterns
        d3 = head $ filter (\x -> length x == 5 && d1 `isSubsequenceOf` x) patterns
        d5 = head $ filter (\x -> length x == 5 && x /= d3 && null (x \\ d9) ) patterns
        d2 = head $ filter (\x -> length x == 5 && x /= d3 && x /= d5 ) patterns
        patternsMap = M.fromList $ zip [d0, d1, d2, d3, d4, d5, d6, d7, d8, d9] [0 .. 9]
```

И даже упрощаем немного условия для понимания

```haskell
solveLine :: ([String], [String]) -> [Int]
solveLine (patterns, digits) = map(patternsMap M.!) digits
    where  
        d1 = head $ filter ((== 2) . length) patterns
        d7 = head $ filter ((== 3) . length) patterns
        d4 = head $ filter ((== 4) . length) patterns
        d8 = head $ filter ((== 7) . length) patterns
        d9 = head $ filter (\x -> length x == 6 && d4 `isSubsequenceOf` x) patterns
        d0 = head $ filter (\x -> length x == 6 && x /= d9 && d1 `isSubsequenceOf` x ) patterns
        d6 = head $ filter (\x -> length x == 6 && x /= d9 && x /= d0 ) patterns
        d3 = head $ filter (\x -> length x == 5 && d1 `isSubsequenceOf` x) patterns
        d5 = head $ filter (\x -> length x == 5 && x /= d3 && x `isSubsequenceOf` d9) patterns
        d2 = head $ filter (\x -> length x == 5 && x /= d3 && x /= d5 ) patterns
        patternsMap = M.fromList $ zip [d0, d1, d2, d3, d4, d5, d6, d7, d8, d9] [0 .. 9]
```