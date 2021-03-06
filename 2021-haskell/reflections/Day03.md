# AoC 2021 Day 3

## Код, который решил задачу

```haskell
module Day03 (solve1, solve2, parse) where

import Data.List (transpose, maximumBy, minimumBy, group, sort, foldl')
import Data.Char (digitToInt)
import Data.Function (on)

parse :: String -> [[Int]]
parse = map (map digitToInt) . lines

solve1 :: [[Int]] -> Int
solve1 l = gamma * epsilon
    where
       gamma = toDec $ map mostCommon $ transpose l
       epsilon = toDec $ map leastCommon $ transpose l

solve2 :: [[Int]] -> Int
solve2 l = oxy * co2
    where
        oxy = toDec $ oxygen $ transpose l
        co2 = toDec $ co2gen $ transpose l

------------------------------------------------
toDec :: [Int] -> Int
toDec = foldl' (\acc x -> acc * 2 + x) 0

leastCommon :: [Int] -> Int
leastCommon = head . minimumBy (compare `on` length) . group . sort

mostCommon :: [Int] -> Int
mostCommon = head . maximumBy (compare `on` length) . group . sort

oxygen :: [[Int]] -> [Int]
oxygen [] = []
oxygen transposed@(x : _) = mC : oxygen filtered
    where
        mC = mostCommon x
        filtered = tail $ transpose $ filter (\num -> head num == mC) $ transpose transposed

co2gen :: [[Int]] -> [Int]
co2gen [] = []
co2gen transposed@(x : _) = lC : co2gen filtered
    where
        lC = leastCommon x
        filtered = tail $ transpose $ filter (\num -> head num == lC) $ transpose transposed
```

Увидев биты, я подумал, что тут возможно будет использовать битовую магию, ушел и потратил полтора часа на поиски того, как работать с битами в хаскеле. Так, ради интереса и на будущее: как применить маску, сделать шифт, как распарсить битовоую строку типа `"0100010"` в число и обратно, и так далее. Пришел невод с одною тиной.

Пришлось отложить биты и работать со строками, чарами и интами.

## Part One

Ищем **гамму**: транспонируем ввод, каждую колонку сортируем, группируем, находим максимальное по длине, смотрим что это, собираем и превращаем в dec:
```haskell
gamma = toDec $ map mostCommon $ transpose input

mostCommon = head . maximumBy (compare `on` length) . group . sort

toDec = foldl' (\acc x -> acc * 2 + x) 0
```

Ищем **эпсилон**: хочу флипнуть инты в гамме, вспоминаю про Bitwise Complement (~), забываю про него, думаю о том, чтобы в map собирать обе цифры, не хочу думать, просто заменяю mostCommon
```haskell
leastCommon = head . minimumBy (compare `on` length) . group . sort
```

Тесты проходят, ответ принят, первая задача решена.
```haskell
solve1 :: [[Int]] -> Int
solve1 l = gamma * epsilon
    where
       gamma = toDec $ map mostCommon $ transpose l
       epsilon = toDec $ map leastCommon $ transpose l
```

TODO:
- [X] Либо собирать и гамму, и эпсилон вместе, либо попробовать эффективно [флипнуть гамму](#флипнуть-гамму).

## Part Two

Очень долго кручу в голове задачу, пытаюсь представить описанный процесс. Получается плохо. Решаю писать буквально, пошагово, переводя текст задачи в код. В общем, как обычно.

Ищем **oxygen**. Начинаем как в первой части, транспонируем > находим самое частое в первой колонке > сохраняем себе > транспонируем обратно > берем только те строчки, которые начинаются с того, что мы нашли > снова транспонируем, выкидываем первую колонку, начинаем заново.

```haskell
oxygen :: [[Int]] -> [Int]
oxygen [] = []
oxygen transposed@(x : _) = mC : oxygen filtered
    where
        mC = mostCommon x
        filtered = tail $ transpose $ filter (\num -> head num == mC) $ transpose transposed
```

Тут запнулся, об условия: 
> **Oxygen**: If 0 and 1 are equally common, keep values with a 1 in the position being considered.
> **CO2**: If 0 and 1 are equally common, keep values with a 0 in the position being considered.

Подумал, что надо переписывать компаратор, учитывая их. Но решил попробовать ничего не делать, проверил в РЕПЛе, работает как надо:
```haskell
(maximumBy (compare `on` length) . group . sort) "00001111"
"1111"

(minimumBy (compare `on` length) . group . sort) "00001111"
"0000"
```

В общем, тесты прошли. Задача решилась.

TODO: 
- вторую задачу бы полностью переписать вместо "кручу-верчу-обмнауть хочу"
- прикрутить бенчмарки ко всей программе 

## Рефактор

### Флипнуть гамму

Накорячим комплемент:
```haskell
solve1 :: [[Int]] -> Int
solve1 l = bitsToDec mostCommons * bitsToDec (map flipBit mostCommons)
    where
       mostCommons = map mostCommon $ transpose l
       
flipBit :: Int -> Int 
flipBit i | i == 0 = 1
          | i == 1 = 0
          | otherwise = error (show i ++ " is not a bit")    
```
Можно, наверное, ввести `data Bit = Zero | One` вместо `Int`

Вторую часть не хочу пока :) думать. Так что после некоторого причесывания получил:

```haskell
module Day03 (solve1, solve2, parse) where

import Data.List (transpose, maximumBy, minimumBy, group, sort)
import Data.Char (digitToInt)
import Data.Function (on)

parse :: String -> [[Int]]
parse = map (map digitToInt) . lines

solve1 :: [[Int]] -> Int
solve1 l = bitsToDec mostCommons * bitsToDec (map flipBit mostCommons)
    where
       mostCommons = map (frequent maximumBy) $ transpose l

solve2 :: [[Int]] -> Int
solve2 l = oxy * co2
    where
        oxy = bitsToDec $ generator (frequent maximumBy) $ transpose l
        co2 = bitsToDec $ generator (frequent minimumBy) $ transpose l

------------------------------------------------

flipBit :: Int -> Int
flipBit i | i == 0 = 1
          | i == 1 = 0
          | otherwise = error (show i ++ " is not a bit")

bitsToDec :: (Foldable t, Num a) => t a -> a
bitsToDec = foldl (\acc x -> acc * 2 + x) 0

frequent :: (Ord a) => (([a] -> [a] -> Ordering) -> [[a]] -> [c]) -> [a] -> c
frequent f = head . f (compare `on` length) . group . sort

generator :: ([Int] -> Int) -> [[Int]] -> [Int]
generator _ [] = []
generator f transposed@(x : _) = lC : generator f filtered
    where
        lC = f x
        filtered = tail $ transpose $ filter (\num -> head num == lC) $ transpose transposed
```

## Benchmarks

```
benchmarking Day3/p1
time                 3.878 ms   (3.677 ms .. 4.028 ms)
                     0.982 R²   (0.963 R² .. 0.995 R²)
mean                 3.759 ms   (3.679 ms .. 3.895 ms)
std dev              339.0 μs   (222.4 μs .. 501.5 μs)
variance introduced by outliers: 59% (severely inflated)
                 
benchmarking Day3/p1 without bitflip. First lazy one
time                 7.714 ms   (7.376 ms .. 8.112 ms)
                     0.989 R²   (0.978 R² .. 0.997 R²)
mean                 7.498 ms   (7.378 ms .. 7.708 ms)
std dev              436.7 μs   (263.9 μs .. 606.0 μs)
variance introduced by outliers: 31% (moderately inflated)
                 
benchmarking Day3/p2
time                 8.418 ms   (8.023 ms .. 8.947 ms)
                     0.983 R²   (0.967 R² .. 0.998 R²)
mean                 8.053 ms   (7.928 ms .. 8.320 ms)
std dev              511.6 μs   (242.8 μs .. 847.0 μs)
variance introduced by outliers: 35% (moderately inflated)
                 
Benchmark criterion-benchmarks: FINISH
```