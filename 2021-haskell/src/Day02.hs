module Day02 (solve1, solve2, parse) where

import Relude (foldl')

data Step
    = Forward Int | Down Int | Up Int
        deriving (Eq, Show, Ord)

data Position
    = Position { xPos :: Int, depth :: Int, aim :: Int}
        deriving (Eq, Show)

parse :: String -> [Step]
parse = map (readStep . words) . lines
    where
        readStep ["up", y] = Up (read y)
        readStep ["down", y] = Down (read y)
        readStep ["forward", y] = Forward (read y)
        readStep sd = error ("Wrong step data" ++ mconcat sd)

solve1 :: [Step] -> Int
solve1 = solve apply1

solve2 :: [Step] -> Int
solve2 = solve apply2

solve :: (Position -> Step -> Position) -> [Step] -> Int
solve f =
    (\(Position x d _) -> x * d) . foldl' f (Position 0 0 0)

apply1 :: Position -> Step -> Position
apply1 pos@Position{..} (Forward x) = pos{xPos = xPos + x}
apply1 pos@Position{..} (Down x) = pos{depth = depth + x}
apply1 pos@Position{..} (Up x) = pos{depth = depth - x}

apply2 :: Position -> Step -> Position
apply2 pos@Position{..} (Forward x) = pos{xPos = xPos + x, depth = depth + aim * x}
apply2 pos@Position{..} (Down x) = pos{aim = aim + x}
apply2 pos@Position{..} (Up x) = pos{aim = aim - x}