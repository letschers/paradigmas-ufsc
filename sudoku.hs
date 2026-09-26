module Main where

import Control.Monad (msum)
import Data.List ((\\))

type Board = [String]

type Cell = (Int, Int)

main :: IO ()
main = case sudokuSolver puzzle of
  Nothing -> putStrLn "no solution"
  Just b -> mapM_ putStrLn b

puzzle :: Board
puzzle =
  [ "53..7....",
    "6..195...",
    ".98....6.",
    "8...6...3",
    "4..8.3..1",
    "7...2...6",
    ".6....28.",
    "...419..5",
    "....8..79"
  ]

-- The one function the outside calls. Same role as solveSudokuRec in Go.
sudokuSolver :: Board -> Maybe Board
sudokuSolver b = go b (empties b)
  where
    -- The recursive part. Same role as the closure `solve` in Go, except the
    -- "index into mutableCords" is the list of cells still left to fill.
    go :: Board -> [Cell] -> Maybe Board
    go board [] = Just board
    go board (cell : rest) =
      msum [go (setCell board cell v) rest | v <- candidates board cell]

-- Same role as mutableCords: every '.' cell in reading order.
empties :: Board -> [Cell]
empties b = [(r, c) | r <- [0 .. 8], c <- [0 .. 8], b !! r !! c == '.']

-- Same role as the three bitmap lookups: digits not yet used in the
-- cell's row, column or box. Recomputed from the board each time.
candidates :: Board -> Cell -> String
candidates b (r, c) = ['1' .. '9'] \\ (rowVals ++ colVals ++ boxVals)
  where
    rowVals = b !! r
    colVals = [b !! i !! c | i <- [0 .. 8]]
    boxVals = [b !! i !! j | i <- [br .. br + 2], j <- [bc .. bc + 2]]
    br = (r `div` 3) * 3
    bc = (c `div` 3) * 3

-- Same role as `board[coord.x][coord.y] = i`, but it returns a new board
-- and leaves the old one untouched. That is what makes the undo step vanish.
setCell :: Board -> Cell -> Char -> Board
setCell b (r, c) v = setAt r (setAt c v (b !! r)) b

setAt :: Int -> a -> [a] -> [a]
setAt i v xs = take i xs ++ [v] ++ drop (i + 1) xs
