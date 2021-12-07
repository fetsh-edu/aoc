# AOC 2021 Day 7

Интуитивно в голову пришло, что для решения первой части должна подходить медиана, но никак не мог себе это объяснить / доказать. Поэтому сначала сделал решение в лоб.

```haskell
parse :: String -> [Int]
parse = map read . splitOn ","

solve1 :: [Int] -> Int
solve1 = solve (\p1 p2 -> abs(p1 - p2))

solve2 :: [Int] -> Int
solve2 = solve (\p1 p2 -> triangular(abs(p1 - p2)))

-----

solve :: (Int -> Int -> Int) -> [Int] -> Int
solve f poss =
    minimum ([0..(maximum poss)] & map (\p -> sum $ map (f p) poss))
```
### Влобный бенч
```
benchmarking Day 7/Puzzle (Day 7) One
time                 12.78 ms   (12.65 ms .. 12.89 ms)
0.999 R²   (0.998 R² .. 1.000 R²)
mean                 13.15 ms   (13.03 ms .. 13.26 ms)
std dev              307.8 μs   (255.3 μs .. 376.7 μs)

benchmarking Day 7/Puzzle (Day 7) Two
time                 11.99 ms   (11.78 ms .. 12.16 ms)
0.999 R²   (0.998 R² .. 1.000 R²)
mean                 12.39 ms   (12.27 ms .. 12.57 ms)
std dev              403.6 μs   (304.4 μs .. 586.2 μs)
variance introduced by outliers: 10% (moderately inflated)
```
### Медиана

Потом попробовал таки проверить гипотезу с медианой, и оно получилось. Решение совпало. Но, подозреваю, что мне тут везет с инпутом: у нас инпут четной длины, и я достаю дальнее из двух медианных значений. 

```haskell
solve1 :: [Int] -> Int
solve1 ints = sum $ map (fuelForPos median) ints
    where median = sort ints !! (length ints `div` 2)
          fuelForPos p1 p2 = abs(p1 - p2)
```
Но хитрого решения для второй части, где там должна быть равноудаленная точка для триангуляров, не придумал. Хорошо, что она решается в лоб, не подвешивая мой компьютер, иначе пришлось бы до вечера сидеть, думать математику, или подсматривать.

### Бенч с медианой
Смущает, что вторая часть стала решаться в четыре раза медленнее, видимо, там всякие оптимизации, и в первом случае, когда решали первую часть, подготовили почву для второй. 
```
benchmarking Day 7/Puzzle (Day 7) One
time                 2.420 ms   (2.389 ms .. 2.440 ms)
                     0.998 R²   (0.995 R² .. 1.000 R²)
mean                 2.495 ms   (2.462 ms .. 2.606 ms)
std dev              198.5 μs   (77.22 μs .. 393.4 μs)
variance introduced by outliers: 57% (severely inflated)
                 
benchmarking Day 7/Puzzle (Day 7) Two
time                 39.54 ms   (39.48 ms .. 39.64 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 39.61 ms   (39.53 ms .. 39.87 ms)
std dev              254.1 μs   (64.30 μs .. 481.3 μs)
```



## Финальный код 
```haskell
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
```
Вторая часть снова вернулась к 10 ms o_O
```
benchmarking Day 7/Puzzle (Day 7) One
time                 2.443 ms   (2.427 ms .. 2.457 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 2.468 ms   (2.454 ms .. 2.503 ms)
std dev              60.40 μs   (27.43 μs .. 103.9 μs)
variance introduced by outliers: 11% (moderately inflated)
                 
benchmarking Day 7/Puzzle (Day 7) Two
time                 10.08 ms   (10.03 ms .. 10.13 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 10.16 ms   (10.12 ms .. 10.21 ms)
std dev              124.1 μs   (89.15 μs .. 182.8 μs)
```