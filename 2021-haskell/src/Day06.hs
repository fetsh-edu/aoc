module Day06  where

import Data.List.Split (splitOn)
import qualified Data.List as L
import qualified Data.Map as M

data Fish = Fish Int Int deriving (Show)

parse :: String -> [Int]
parse = map read . splitOn ","

solve1 :: [Int] -> Int
solve1 = length . fishesAfter 80 . map (Fish 7)
---- or: sum . map (fishesAfter' 256)

solve2 :: [Int] -> Int
solve2 = fishesAfter'' 256

-- PART 1 --------------------------------

fishesAfter :: Int -> [Fish] -> [Fish]
fishesAfter days fishes = foldl spawn fishes [1..days]

spawn :: [Fish] -> Int -> [Fish]
spawn fishes _ = map age fishes ++ replicate (length $ filter (\(Fish _ d) -> d == 0) fishes) (Fish 9 8)
    where age (Fish base days) = Fish base (mod (days - 1) (if days == 0 then 7 else base))

fishesAfter' :: Int -> Int -> Int
fishesAfter' days age =
    1 + sum (map (\i -> fishesAfter' (days - i) 8) $ take children $ iterate (+7) (age + 1))
    where children = (7 - age + (days - 1))  `div` 7

-- PART 2 --------------------------------

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