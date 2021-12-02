module Day02 (Step (..), solve1, solve2, parse) where

import Relude ((&))

parse :: String -> [Step]
parse = fmap readStep . lines

solve1 :: [Step] -> Int
solve1 ps = ps & foldr apply1 (Position 0 0) & multiply

solve2 :: [Step] -> Int
solve2 ps = ps & foldr apply2 (Position 0 0) & multiply


data Step
    = Forward Int
    | Down Int
    | Up Int
    deriving (Eq, Show, Ord)

data Position = Position {
    xPosition :: Int,
    depth :: Int
} deriving (Eq, Show)

readStep :: String -> Step
readStep s =
    s & words & readStep'
    where
      readStep' ["up", y] = Up (read y)
      readStep' ["down", y] = Down (read y)
      readStep' ["forward", y] = Forward (read y)

multiply :: Position -> Int
multiply (Position x z) = x * z

apply1 :: Step -> Position -> Position
apply1 (Forward sx) (Position px z) = Position (px + sx) z
apply1 (Down sz) (Position px pz) = Position px (pz + sz)
apply1 (Up sz) (Position px pz) = Position px (pz - sz)

apply2 :: Step -> Position -> Position
apply2 (Forward sx) (Position px z) = Position (px + sx) z
apply2 (Down sz) (Position px pz) = Position px (pz + sz)
apply2 (Up sz) (Position px pz) = Position px (pz - sz)