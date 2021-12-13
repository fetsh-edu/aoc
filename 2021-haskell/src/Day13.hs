module Day13 (solve1, solve2, parse) where

import qualified Data.Set as S (Set, fromList, size, map, member)
import AOC.Utils (splitOn, both)
import Data.Matrix (matrix, Matrix)
data Instruction = X Int | Y Int

parse :: String -> (S.Set (Int, Int), [Instruction])
parse input = (points, map parseInstruction instructions')
    where
      (points', instructions') = splitOn [""] $ lines input
      points = S.fromList (map (both read . splitOn ",") points')
      parseInstruction ('f':'o':'l':'d':' ':'a':'l':'o':'n':'g':' ':'y':'=':y) = Y (read y)
      parseInstruction ('f':'o':'l':'d':' ':'a':'l':'o':'n':'g':' ':'x':'=':x) = X (read x)

solve1 :: (S.Set (Int, Int), [Instruction]) -> Int
solve1 (pSet, instructions) =
     S.size $ scanl (\ps ins -> S.map (applyInstruction ins) ps) pSet instructions !! 1

solve2 :: (S.Set (Int, Int), [Instruction]) -> Matrix Char
solve2 (pSet, instructions) = matrix'
    where final = foldl (\ps ins -> S.map (applyInstruction ins) ps) pSet instructions
          rows = (+1) $ maximum $ S.map snd final
          cols = (+1) $ maximum $ S.map fst final
          matrix' = matrix rows cols $ \(j,i) -> if S.member (i-1,j-1) final then '#' else ' '

---------------------

applyInstruction :: Instruction -> (Int, Int) -> (Int, Int)
applyInstruction (Y int) (x, y) = if y < int then (x, y) else (x, int - (y - int))
applyInstruction (X int) (x, y) = if x < int then (x, y) else (int - (x - int), y)