module AOC.Utils where

import Data.List (tails, sort, group, minimumBy, maximumBy, find)
import Data.Function (on)
import qualified Data.List.Split as DLS 
import Data.Maybe (fromMaybe)


triangular :: Integral a => a -> a
triangular num = (num * (num + 1)) `div` 2

unsafeFind :: Foldable t => (a -> Bool) -> t a -> a
unsafeFind p = fromMaybe (error "AAAAAAAAa") . find p

--median :: Ord a => [a] -> a
--median x =
--   if odd n
--     then sort x !! (n `div` 2)
--     else sort x !! (n `div` 2 - 1)
--    where n = length x

medianR :: (Ord a, Fractional a) => [a] -> a
medianR x =
   if odd n
     then sort x !! (n `div` 2)
     else ((sort x !! (n `div` 2 - 1)) + (sort x !! (n `div` 2))) / 2
    where n = length x


loop :: (Eq t1, Num t1) => t1 -> (t2 -> t2) -> t2 -> t2
loop 0 _ el = el
loop n f el = f (loop (n-1) f el)

splitOn :: Eq a => [a] -> [a] -> ([a], [a])
splitOn xs xss = (head p, p !! 1) where p = DLS.splitOn xs xss

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

range :: (Num a, Enum a) => a -> a -> [a]
range x y = [x, (x - signum (x - y))..y]

range' :: (Eq a, Num a, Enum a) => a -> a -> [a]
range' x y | x == y = [x..y]
           | otherwise = [x, (x - signum (x - y))..y]

slides :: Int -> [a] -> [[a]]
slides n = slidesWith n (:) []

slidesWith :: Int -> (a -> b -> b) -> b -> [a] -> [b]
slidesWith n f acc = foldr (zipWith f) (repeat acc) . take n . tails

bitsToDec :: (Foldable t, Num a) => t a -> a
bitsToDec = foldl (\acc x -> acc * 2 + x) 0

mostCommon :: (Ord a) => [a] -> a
mostCommon = frequent maximumBy

leastCommon :: (Ord a) => [a] -> a
leastCommon = frequent minimumBy

frequent :: (Ord a, Foldable t) => ((t a1 -> t a1 -> Ordering) -> [[a]] -> [c]) -> [a] -> c
frequent f = head . f (compare `on` length) . group . sort