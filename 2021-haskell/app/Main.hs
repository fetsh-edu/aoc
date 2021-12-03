module Main where

import System.Environment
import AOC.Puzzles

main :: IO ()
main = do
    args <- getArgs
    let puzzle = argsToPuzzle args
    input <- 
        case fileNameFor <$> puzzle of
            Left t -> fail t
            Right p -> readFile p
    case doPuzzle input <$> puzzle of
      Left t -> putStrLn t
      Right a -> putStrLn a