module Lib.Utils where

import Data.List (tails)

slides :: Int -> [a] -> [[a]]
slides n = slidesWith n (:) []

slidesWith :: Int -> (a -> b -> b) -> b -> [a] -> [b]
slidesWith n f acc = foldr (zipWith f) (repeat acc) . take n . tails
