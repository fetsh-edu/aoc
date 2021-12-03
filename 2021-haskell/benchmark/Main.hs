module Main where

import System.Environment
import Criterion.Main
import AOC.Puzzles

main :: IO ()
main = do
    args <- getArgs
    let day = Day (read $ head args)
    let puzzle1 = Puzzle day One
    let puzzle2 = Puzzle day Two
    input <- readFile $ fileNameFor puzzle1
    let func1 = getPuzzleUnsafe puzzle1
    let func2 = getPuzzleUnsafe puzzle2
    print $ head input
    withArgs (drop 2 args) $ defaultMain
        [ bgroup (show day)
            [ bench (show puzzle1) $ whnf func1 input
            , bench (show puzzle2) $ whnf func2 input
            ]
        ]