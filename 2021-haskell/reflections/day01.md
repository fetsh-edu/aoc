# AoC 2021 Day 1

## Refactored

Прикольно видеть, что за цифры перед тобой, и соображать, что окно из трех не нужно и не надо ничего складывать.

Финальный код после изучений чужих решений:
```haskell
solve1 :: [Int] -> Int
solve1 = solve 1

solve2 :: [Int] -> Int
solve2 = solve 3

solve :: Int -> [Int] -> Int
solve int input =
    zipWith (<) input (drop int input) & filter id & length
```

## Initial 


Сделать быстренько sliding window из двух элементов -- легко: zip листа с tail листа. Но из трех элементов тем же способом (zip листа с tail листа с tail tail листа) (never mind, its called drop 2 :) -- тоже быстренько, но некрасиво и неудобно. Никаких готовых функций, делающих мне sliding winow на хугле не нашлось.

Начал писать рукурсивный метод, все дела, но тут нашлось такое: https://stackoverflow.com/questions/27726739/implementing-an-efficient-sliding-window-algorithm-in-haskell

```haskell
import Control.Applicative
import Data.Traversable (sequenceA)
import Data.List (tails)

transpose' :: [[a]] -> [[a]]
transpose' = getZipList . sequenceA . map ZipList
```

Zipping a list of lists is just a [transposition](https://en.wikipedia.org/wiki/Transpose), but unlike `transpose` from `Data.List` it throws away outputs that would have less than _n_ elements.

Now it's easy to make the window function: Take _m_ lists, each shifted by 1, and just zip them:

```haskell
windows :: Int -> [a] -> [[a]]
windows m = transpose' . take m . tails
```

И дальше из комментариев:  
or `foldr (zipWith (:)) (repeat []) . take m . tails` that's just what the `sequenceA` on `ZipList`


Решил сразу складывать (вычитать), зипуя:
```haskell
nums & tails & take 2 & foldr (zipWith (-)) (repeat 0) & filter (< 0) & length
```

Но для трех уже глупость получилась, сначала складываю по три, потом снова возвращаюсь к первой задаче:
```haskell
nums  
    & tails  
    & take 3
    & foldr (zipWith (+)) (repeat 0)  
    & tails  
    & take 2  
    & foldr (zipWith (-)) (repeat 0)  
    & filter (< 0) & length & show
```

Лепим всё это вместе и получаем

```haskell
module Day01 (solve1, solve2) where

import Relude ((&))

import Data.List (tails)

solve1 :: String -> String
solve1 input =
    input
        & ints
        & slides 2
        & filter (\x -> x !! 1 > head x) & length & show

solve2 :: String -> String
solve2 input =
    input
        & ints
        & slides 3
        & slides 2
        & filter (\x -> sum (x !! 1) > sum (head x)) & length & show

ints :: String -> [Int]
ints = map read . lines

slides :: Int -> [a] -> [[a]]
slides n =
     foldr (zipWith (:)) (repeat []) . take n . tails
```

Не очень нравится, да и, честно говоря, про `sequence`, `transposition` и вот это вот `foldr (zipWith (+)) (repeat 0)` надо бы лучше понять.

Наверняка простой фолд по инпуту с подсчетом был бы проще и лучше.

