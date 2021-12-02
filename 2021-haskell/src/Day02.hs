module Day02 (Step (..), solve1, solve2, apply2, parse, Position (..)) where

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
apply1 (Position xP d a) (Forward x) = Position (xP + x) d a
apply1 (Position xP d a) (Down x) = Position xP (d + x) a
apply1 (Position xP d a) (Up x) = Position xP (d - x) a

apply2 :: Position -> Step -> Position
apply2 (Position xP d a) (Forward x) = Position (xP + x) (d + a * x) a
apply2 (Position xP d a) (Down x) = Position xP d (a + x)
apply2 (Position xP d a) (Up x) = Position xP d (a - x)