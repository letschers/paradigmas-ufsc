module Main where

import Control.Monad (msum)
import Data.List (nub, permutations, transpose, (\\))

type Grid = [[Int]]

-- Edge clues, read left to right for top/bottom and top to bottom for
-- left/right. 0 means the clue is not given.
data Clues = Clues
  { top :: [Int],
    bottom :: [Int],
    left :: [Int],
    right :: [Int]
  }

main :: IO ()
main = case solve puzzle5 of
  Nothing -> putStrLn "no solution"
  Just g -> mapM_ (putStrLn . unwords . map show) g

-- 5x5 from janko.at, 12 of 20 clues given.
puzzle5 :: Clues
puzzle5 =
  Clues
    { top = [0, 3, 3, 0, 0],
      bottom = [0, 2, 2, 2, 1],
      left = [1, 4, 3, 2, 0],
      right = [3, 2, 3, 0, 1]
    }

-- How many buildings are visible looking along the list from its head.
-- A building is visible when it is taller than everything before it.
visible :: [Int] -> Int
visible = visibleFrom 0

-- Same, but pretend a building of the given height already stands in front.
visibleFrom :: Int -> [Int] -> Int
visibleFrom = go 0
  where
    go count _ [] = count
    go count tallest (h : hs)
      | h > tallest = go (count + 1) h hs
      | otherwise = go count tallest hs

-- Does a line satisfy a clue? A clue of 0 always does.
matches :: Int -> [Int] -> Bool
matches 0 _ = True
matches c line = visible line == c

-- Every arrangement of 1..n that satisfies a left clue and a right clue.
-- Same role as `candidates` in the sudoku, but a candidate is a whole row.
rowOptions :: Int -> Int -> Int -> [[Int]]
rowOptions n l r =
  [p | p <- permutations [1 .. n], matches l p, matches r (reverse p)]

-- Can a column whose top part is placed still end up matching its top clue?
-- What is seen so far can only grow, and by at most one per height taller
-- than the tallest placed so far. On a complete column this is exact.
couldMatchTop :: Int -> Int -> [Int] -> Bool
couldMatchTop _ 0 _ = True
couldMatchTop n t col = seen <= t && t <= seen + (n - maximum (0 : col))
  where
    seen = visible col

-- Same question for the bottom clue. Looking up from the bottom, the
-- unplaced cells come first. We do not know their order, but we know which
-- heights they are (the ones missing from the column), so we know the
-- tallest of them. The unplaced part shows between 1 and `missing`
-- buildings; after it the placed part contributes a fixed amount.
-- On a complete column this is exact.
couldMatchBottom :: Int -> Int -> [Int] -> Bool
couldMatchBottom _ 0 _ = True
couldMatchBottom n b col = fixed + lo <= b && b <= fixed + hi
  where
    missing = n - length col
    tallestMissing = if missing == 0 then 0 else maximum ([1 .. n] \\ col)
    fixed = visibleFrom tallestMissing (reverse col)
    lo = min 1 missing
    hi = missing

-- Same shape as the sudoku solver: `go` places one row per step instead of
-- one cell. After each row every column is checked for repeats and for
-- whether its top and bottom clues are still reachable, so dead ends are
-- cut early. On the last row those checks become exact, so the base case
-- has nothing left to verify.
solve :: Clues -> Maybe Grid
solve clues = go [] rowOpts
  where
    n = length (top clues)

    -- computed once: the surviving permutations for each row, top to bottom
    rowOpts = [rowOptions n l r | (l, r) <- zip (left clues) (right clues)]

    go :: Grid -> [[[Int]]] -> Maybe Grid
    go rows [] = Just rows
    go rows (opts : rest) =
      msum [go rows' rest | p <- opts, let rows' = rows ++ [p], columnsPossible rows']

    columnsPossible rows =
      and
        [ length (nub col) == length col
            && couldMatchTop n t col
            && couldMatchBottom n b col
          | (t, b, col) <- zip3 (top clues) (bottom clues) (transpose rows)
        ]
