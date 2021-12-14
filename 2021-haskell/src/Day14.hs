module Day14 (solve1, solve2, parse) where

import qualified Data.Map as M
import AOC.Utils (splitOn, loop)

type Rules = M.Map String [String]
type Counter = M.Map String Integer

parse :: String -> (Counter, Rules)
parse input = (counter, rules)
    where ([template], rules') = splitOn [""] $ lines input
          rules = M.fromList $ map ruleToPair rules'
          ruleToPair [a, b, ' ', '-', '>', ' ', c] = ([a,b], [[a,c], [c,b]])
          counter = M.fromListWith (+) $ zipWith (\a b -> ([a,b], 1)) (' ' : template) template

solve1 :: (Counter, Rules) -> Integer
solve1 (counter, rules) = solve 10 counter rules

solve2 :: (Counter, Rules) -> Integer
solve2 (counter, rules) = solve 40 counter rules

-----------------------------------

solve :: Int -> Counter -> Rules -> Integer
solve i cnt rules = mostCommon - leastCommon
    where cnt' = compact $ loop i (grow rules) cnt
          mostCommon = maximum $ M.elems cnt'
          leastCommon = minimum $ M.elems cnt'

compact :: Counter -> M.Map Char Integer
compact = M.fromListWith (+) . map (\([f, s], c) -> (s, c)) . M.toList

grow :: Rules -> Counter -> Counter
grow rules cnt = M.fromListWith (+) $ concatMap grow' $ M.toList cnt
    where grow' (str, c) = case rules M.!? str of
                           Just some -> map (,c) some
                           Nothing -> [(str, c)]