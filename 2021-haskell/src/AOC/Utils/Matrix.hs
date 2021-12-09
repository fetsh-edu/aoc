module AOC.Utils.Matrix where

import AOC.Utils (neighbors')

neighbors :: Int -> Int -> Int -> Int -> [(Int, Int)]
neighbors r c = neighbors' r c 1 1

