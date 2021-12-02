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
apply1 (Position px z a) (Forward sx) = Position (px + sx) z a
apply1 (Position px pz a) (Down sz) = Position px (pz + sz) a
apply1 (Position px pz a) (Up sz) = Position px (pz - sz) a

apply2 :: Position -> Step -> Position
apply2 (Position xP depth a) (Forward x) = Position (xP + x) (depth + a * x) a
apply2 (Position px pz a) (Down sz) = Position px pz (a + sz)
apply2 (Position px pz a) (Up sz) = Position px pz (a - sz)