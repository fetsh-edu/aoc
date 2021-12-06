module Day06  where

import Data.List.Split (splitOn)
import qualified Data.List as L
import qualified Data.Map as M
import AOC.Utils (loop) 

parse :: String -> [Int]
parse = map read . splitOn ","

solve1 :: [Int] -> Int
solve1 = fishesAfter 80

solve2 :: [Int] -> Int
solve2 = fishesAfter 256

fishesAfter :: Int -> [Int] -> Int
fishesAfter days fishes = sum $ M.elems $ loop days spawn fishesCounter
    where
        fishesCounter = M.fromList $ map (\x -> (head x, length x)) $ L.group $ L.sort fishes
        spawn acc = M.fromList
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