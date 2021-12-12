module AOC.Utils.Matrix where

import AOC.Utils (neighbors')
import Data.Ix (inRange, Ix, range)



-- Returns four neighbours
neighbors :: Int -> Int -> Int -> Int -> [(Int, Int)]
neighbors r c = neighbors' r c 1 1

-- returns 9 neighbours excluding itself
neighbors1 :: (Num b, Enum b, Ix b) => b -> b -> b -> b -> [(b, b)]
neighbors1 r c maxR maxC = [ (r', c')
                    | r' <- [r-1..r+1], c' <- [c-1..c+1]
                    , (r', c') /= (r, c)
                    , inRange ((1,1), (maxR, maxC)) (r', c') ]

-- returns 9 neighbours including itself
neighbors2 :: (Num b, Enum b, Ix b) => b -> b -> b -> b -> [(b, b)]
neighbors2 r c maxR maxC = [ (r', c')
                    | r' <- [r-1..r+1], c' <- [c-1..c+1]
                    , (r', c') /= (r, c)
                    , inRange ((1,1), (maxR, maxC)) (r', c') ]

-- returns 9 neighbours including itself. same as prev
neighbors3 :: (Ix a, Num a) => a -> a -> a -> a -> [(a, a)]
neighbors3 r c maxR maxC =
    filter (inRange ((1, 1), (maxR, maxC))) $ map (\(r', c') -> (r' + r, c' + c)) (range ((-1, -1), (1,1)))
