# AOC 2021 Day 14

First take which solves part one but fails on part two obviously. 

```haskell
module Day14 (solve1, solve2, parse) where

import qualified Data.Map as M
import AOC.Utils (splitOn, mostCommon', leastCommon', slidesWith, slides, loop)
import Debug.Trace (trace)

type Template = String
type Rules = M.Map String Char

parse :: String -> (Template, Rules)
parse input = (template, rules)
    where ([template], rules') = splitOn [""] $ lines input
          rules = M.fromList $ map ruleToPair rules'
          ruleToPair [a, b, ' ', '-', '>', ' ', c] = ([a,b], c)

solve1 :: (Template, Rules) -> Int
solve1 (template, rules) = mostCommon - leasCommon
    where mostCommon = length $ mostCommon' polymer
          leasCommon = length $ leastCommon' polymer
          polymer = loop (10 :: Int) (grow rules) template

solve2 :: (Template, Rules) -> Int
solve2 (template, rules) = mostCommon - leasCommon
    where mostCommon = length $ mostCommon' polymer
          leasCommon = length $ leastCommon' polymer
          polymer = loop (40 :: Int) (grow rules) template

-----------------------------------

grow :: Rules -> Template -> String
grow rules (f:s:[]) = [f, rules M.! [f,s], s]
grow rules (f:s:rest) = [f, rules M.! [f,s]] ++ grow rules (s:rest)
```