# AOC 2021 Day 8

## Код, который решил задачу
```haskell
parse :: String -> [[Int]]
parse = map (map digitToInt) . lines

solve1 :: [[Int]] -> Int
solve1 = sum . map ((+1) . snd) . lowPoints . M.fromLists

solve2 :: [[Int]] -> Int
solve2 input = product . take 3 . reverse . sort
               . map (Set.size . (getBasin matrix Set.empty . fst))
               $ lowPoints matrix
               where matrix = M.fromLists input

-------------------------

infoPoint :: Ord a => M.Matrix a -> (Int, Int) -> a -> (((Int, Int), a), Bool)
infoPoint matrix (r, c) a = (((r, c), a), isLow)
    where isLow = all ((> a) . (matrix M.!)) neighbors'
          neighbors' = M.neighbors r c (M.nrows matrix) (M.ncols matrix)
    
lowPoints :: M.Matrix Int -> [((Int, Int), Int)]
lowPoints matrix =
    map fst $ filter snd $ M.toList $ M.mapPos (infoPoint matrix) matrix

getBasin :: M.Matrix Int -> Set.Set (Int, Int) -> (Int, Int) -> Set.Set (Int, Int)
getBasin matrix set point@(r,c) =
    foldl (getBasin matrix) newSet (filter inBasin allNeighbors)
    where
       newSet = Set.insert point set
       allNeighbors = M.neighbors r c (M.nrows matrix) (M.ncols matrix)
       inBasin p = matrix M.! p /= 9 && Set.notMember p newSet
```
