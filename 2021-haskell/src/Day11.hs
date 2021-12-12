module Day11 where

import Data.Char (digitToInt)
import qualified Data.Map as M
import AOC.Utils.Matrix (neighbors1)
import Data.Ix (range)

parse :: String -> Array
parse = M.fromList . zip (range ((1,1), (10,10))) . map digitToInt . mconcat . lines

solve1 :: Array -> Int
solve1 i = sum $ take 101 $ map snd $ iterate step (i, 0)

solve2 :: Array -> Int
solve2 i = length $ takeWhile ((/= 100) . snd) $ iterate step (i, 0)

-------------------------------------------------
type Point = (Int, Int)
type Array = M.Map Point Int

step :: (Array, Int) -> (Array, Int)
step (matrix, _) = (matrix'', length flashed)
    where initStep = fmap succ matrix
          (matrix', flashed) = flash initStep []
          matrix'' = fmap (\v -> if v > 9 then 0 else v) matrix'


flash :: Array -> [Point] -> (Array, [Point])
flash matrix flashed =
    let flashed' = [p | (p, v) <- M.assocs matrix, v > 9, p `notElem` flashed]
        neighbors' = (\(r,c) -> neighbors1 r c 10 10) =<< flashed'
        matrix' = foldl (\m p -> M.insertWith (+) p 1 m ) matrix neighbors'
    in
        if null flashed' then (matrix, flashed)
        else flash matrix' (flashed' ++ flashed)