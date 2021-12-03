module Main where

import System.Environment
import Criterion.Main
import AOC.Puzzles

main :: IO ()
main = do
    args <- getArgs
    let puzzle = case argsToPuzzle args of
                    Left e -> error e
                    Right p -> p
    input <- readFile $ fileNameFor puzzle
    let func = getPuzzleUnsafe puzzle
    print $ head input
    withArgs (drop 2 args) $ defaultMain [bench (show puzzle) $ whnf func input ]