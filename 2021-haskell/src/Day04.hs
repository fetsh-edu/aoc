module Day04 (solve1, solve2, parse) where

import Data.Matrix
import Data.List.Split
import Data.Foldable (find)

data Number = Drawn Int | NotDrawn Int deriving (Eq)
data Board = Bingo (Matrix Number) | NoBingo (Matrix Number)

parse :: String -> ([Int], [Board])
parse a = (drawn, boards)
    where
        drawn = map read $ splitOn "," $ head inputLines
        boards = map (NoBingo . Data.Matrix.fromLists . map (map (NotDrawn . read) . words) . take 5) $ chunksOf 6 $ drop 2 inputLines
        inputLines = lines a

solve1 :: ([Int], [Board]) -> Maybe Int
solve1 (draws, boards) = score <$> findWinner boards draws

solve2 :: ([Int], [Board]) -> Maybe Int
solve2 (draws, boards) = score <$> findLooser boards draws

-----------------------------------------------
score :: (Int, Board) -> Int
score (int, board) =
    (* int) . sum . map getNum . filter (not . isDrawn) . Data.Matrix.toList . getBoard $ board

findWinner :: [Board] -> [Int] -> Maybe (Int, Board)
findWinner [] _ = Nothing
findWinner _ [] = Nothing
findWinner boards (num : pendingDraws) = result
    where
        drawnBoards = map (draw num) boards
        result = case find isBingo drawnBoards of
                Just something -> Just (num, something)
                Nothing -> findWinner drawnBoards pendingDraws

findLooser :: [Board] -> [Int] -> Maybe (Int, Board)
findLooser [] _ = Nothing
findLooser _ [] = Nothing
findLooser boards (num : pendingDraws) = result
    where
        drawnBoards = map (draw num) boards
        noBingoBoards = filter (not . isBingo) drawnBoards
        result = if null noBingoBoards
                    then Just(num, head $ filter isBingo drawnBoards)
                    else findLooser noBingoBoards pendingDraws

draw :: Int -> Board -> Board
draw num board =
    foldl (f num) board [(x,y) | x <- [1..5], y <- [1..5]]
    where
        f :: Int -> Board -> (Int, Int) -> Board
        f n b (x, y) =
            case b of
            Bingo _ -> b
            NoBingo m ->
             if unsafeGet x y m == NotDrawn n then
               let
                 drawnBoard = unsafeSet (Drawn n) (x, y) m
               in
                 if all isDrawn (getRow x drawnBoard) || all isDrawn (getCol y drawnBoard) then
                     Bingo drawnBoard
                 else
                     NoBingo drawnBoard
             else
               NoBingo m

------------------------------------

isDrawn :: Number -> Bool
isDrawn (Drawn _) = True
isDrawn _ = False

isBingo :: Board -> Bool
isBingo (Bingo _) = True
isBingo _ = False

getBoard :: Board -> Matrix Number
getBoard (Bingo a) = a
getBoard (NoBingo a) = a

getNum :: Number -> Int
getNum (NotDrawn a) = a
getNum (Drawn a) = a
