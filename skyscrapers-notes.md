# skyscrapers.hs, explained for the presentation

Line numbers refer to the current `skyscrapers.hs` (309 lines, Portuguese comments). Try every ghci example, it is the fastest way to make each piece stick.

## The idea in three sentences

Every row must be an ordering of 1..n that satisfies its left and right clue (and any cells given in the statement), so first compute those orderings for each row. Then place rows one at a time, top to bottom, and after each placement check that every column so far is still the beginning of some ordering allowed by that column's top and bottom clues. If a row fails that check, try the next ordering for it; if a row runs out of orderings, go back one row and try its next ordering.

That is backtracking. The "go back" costs nothing here because nothing was ever modified.

---

## Part 1: every definition

### Lines 39 to 44, the types

```haskell
type Grid = [[Int]]
data Clues = Clues [Int] [Int] [Int] [Int]
```

`type` is only a nickname. `Grid` and `[[Int]]` are the same type, so `[[5,3],[1,2]]` is a `Grid` without saying so. A `Grid` is used for two things: a solution, and the cells given in the statement, where 0 means "empty".

`data` creates a new type with one constructor, also called `Clues`, holding four lists in a fixed order: top, bottom, left, right. To build one you write `Clues [..] [..] [..] [..]` (line 236). To take one apart you pattern match `(Clues top bottom left right)` (line 173). A 4-tuple would have worked too. The constructor gives it a name and stops you from passing some unrelated 4-tuple by mistake.

A clue of `0` means "not given". Puzzles from janko leave clues out, so this is needed from the first real board.

### Lines 52 to 57, `main`

```haskell
main = do
  run "..." puzzle5 (emptyGrid 5) solution5
  run "..." janko003 (emptyGrid 6) solution003
  ...
```

The only entry point. Four boards from the janko page, each solved and checked against its published solution. Nothing in the file runs unless `main` needs it.

### Lines 203 to 224, output

```haskell
run name clues givens expected = do
  putStrLn ("== " ++ name)
  printResult (solve clues givens) expected
  putStrLn ""
```

`solve clues givens` is written here but evaluated only when `printResult` needs to know if it is `Nothing` or `Just`.

```haskell
printResult Nothing _ = putStrLn "sem solucao"
printResult (Just grid) expected = do
  printRows grid
  if grid == expected then ... else ...
```

Two equations, one per shape of `Maybe`. This pattern match is what triggers the solver. `==` on lists compares element by element, so `grid == expected` is the whole check against the published solution.

```haskell
printRows [] = return ()
printRows (row : rows) = do
  print row
  printRows rows
```

Recursion over a list inside IO. `return ()` is the "do nothing" IO action, needed because every equation must produce an `IO ()`. Same shape as `lerDo` on the monads slides.

### Lines 66 to 76, visibility

```haskell
visible line = visibleAfter 0 line

visibleAfter _ [] = 0
visibleAfter tallest (h : hs)
  | h > tallest = 1 + visibleAfter h hs
  | otherwise = visibleAfter tallest hs
```

The rule of the puzzle: a building is seen if it is taller than everything before it. `visibleAfter` carries "the tallest so far". Start it at 0 so the first building is always seen. When `h` is taller, count it and it becomes the new tallest. Otherwise skip it.

```
visible [2,1,4,3]   -- 2   (2 and 4)
visible [1,2,3,4]   -- 4
visible [4,3,2,1]   -- 1
```

### Lines 79 to 81, `matches`

```haskell
matches 0 _ = True
matches clue line = visible line == clue
```

Clue 0 accepts anything. Equations are tried top to bottom, so the `0` case must come first.

### Lines 93 to 95, `perms`

```haskell
perms [] = [[]]
perms xs = [x : rest | x <- xs, rest <- perms (xs \\ [x])]
```

Every ordering of a list. Read the comprehension as: pick any `x` from the list, then any ordering `rest` of what is left, and put `x` in front.

`xs \\ [x]` is list difference from `Data.List`: it removes the first occurrence of `x`.

The base case is `[[]]`, one ordering of nothing, not `[]`, zero orderings. With `[]` the comprehension would produce nothing for every input, because there would be no `rest` to pick.

```
perms [1,2,3]          -- [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]
length (perms [1..6])  -- 720
```

### Lines 101 to 103, `lineOptions`

```haskell
lineOptions n front back =
  [p | p <- perms [1 .. n], matches front p, matches back (reverse p)]
```

All orderings of 1..n that satisfy the clue at the front and the clue at the back. The back clue is checked on the reversed list, because that is what you see walking in from the other end.

The same function serves rows (left, right) and columns (top, bottom). That is why it says `front` and `back`, not `left` and `right`.

```
lineOptions 4 1 4              -- [[4,3,2,1]]
lineOptions 4 4 1              -- [[1,2,3,4]]
length (lineOptions 5 1 3)     -- 11, row 0 of puzzle5
length (lineOptions 6 0 0)     -- 720, no filtering
```

### Lines 107 to 110, `fitsGivens`

```haskell
fitsGivens [] _ = True
fitsGivens _ [] = True
fitsGivens (g : gs) (x : xs) = (g == 0 || g == x) && fitsGivens gs xs
```

Does a candidate row respect the cells already filled in the statement? Walk both lists together; where the given is 0 anything goes, otherwise the heights must match. Puzzles 42 and 43 from janko have two given cells each.

```
fitsGivens [0,5,0,0,0,0] [4,5,3,2,1,6]   -- True
fitsGivens [0,5,0,0,0,0] [4,3,5,2,1,6]   -- False
```

### Lines 117 to 118, `column`

```haskell
column rows c = [row !! c | row <- rows]
```

Element `c` of each row, top to bottom. With three rows placed it returns a list of three.

### Lines 123 to 126, `isPrefix`

```haskell
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (x : xs) (y : ys) = x == y && isPrefix xs ys
```

"Is the first list the beginning of the second." The empty list is the beginning of everything, nothing non-empty is the beginning of the empty list, otherwise compare heads and recurse. The order of the first two equations matters: `isPrefix [] []` must be `True`.

### Lines 132 to 133, `stillPossible`

```haskell
stillPossible options partial = [o | o <- options, isPrefix partial o] /= []
```

Is there at least one allowed column that begins with what has been placed? Builds the list of matching options and asks if it is non-empty. Because of laziness, `/= []` only needs to know whether the list has a first element, so the search stops at the first match.

When the column is complete, a prefix of the same length is equality, so this becomes an exact check. That is why `solve` has nothing left to verify at the end.

### Lines 136 to 138, `allTrue`

```haskell
allTrue [] = True
allTrue (b : bs) = b && allTrue bs
```

Are all booleans true. `&&` stops at the first `False`. The Prelude function `and` does the same; this one is written out to match the class material.

### Lines 148 to 151, `firstJust`

```haskell
firstJust [] = Nothing
firstJust (Just x : _) = Just x
firstJust (Nothing : rest) = firstJust rest
```

The heart of the search. Given a list of attempts, return the first that succeeded, or `Nothing` if none did.

The list it receives is lazy. Its elements are recursive calls to `go`, and each is evaluated only when `firstJust` reaches it. So when the second equation fires, every attempt after `x` has never been computed. The search stops at the first solution without any explicit "stop" instruction.

### Lines 172 to 195, `solve`

```haskell
solve (Clues top bottom left right) givens = go [] rowOpts
  where
    n = length top
    rowOpts = [rowOptions i | i <- [0 .. n - 1]]
    colOpts = [lineOptions n (top !! i) (bottom !! i) | i <- [0 .. n - 1]]

    rowOptions i =
      [p | p <- lineOptions n (left !! i) (right !! i), fitsGivens (givens !! i) p]
```

Pattern match the clues into four lists; `givens` is the grid of given cells. `n` is the grid size. `rowOpts` holds, for each row, its allowed orderings after both filters: the two clues and the given cells. `colOpts` the same for each column, clues only. Being `where` bindings of `solve`, they are computed once per call and shared by every `go` underneath.

```haskell
    go rows [] = Just rows
    go rows (opts : rest) = firstJust [tryRow (rows ++ [p]) rest | p <- opts]
```

`go` takes the rows placed so far and the option lists for the rows still to place. No rows left means every row is placed and the column checks along the way were exact on the last row, so `rows` is a solution. Otherwise, for each ordering `p` allowed for the next row, try it, and keep the first attempt that works.

The type `[[[Int]]]` reads as: a list (one per remaining row) of lists (the orderings) of lists of Int (one ordering).

```haskell
    tryRow rows rest
      | columnsOk rows = go rows rest
      | otherwise = Nothing
```

One attempt. If the columns are still fine, recurse one row deeper. If not, this attempt is `Nothing` and `firstJust` moves on.

```haskell
    columnsOk rows =
      allTrue [stillPossible (colOpts !! c) (column rows c) | c <- [0 .. n - 1]]
```

For every column index, the column so far must be a prefix of some allowed column.

### Lines 231 to 309, the boards

`emptyGrid n` builds an n by n grid of zeros for puzzles with no given cells. Then four boards, each as a `Clues` value, an optional grid of givens, and the solution published on janko.at:

| Name | Board | Clues | Given cells |
|---|---|---|---|
| `puzzle5` | janko 5x5 | 12 of 20 | none |
| `janko003` | Nr. 3, Tim Peeters | 24 of 24 | none |
| `janko042` | Nr. 42, Otto Janko | 12 of 24 | 2 |
| `janko043` | Nr. 43, Otto Janko | 12 of 24 | 2 |

---

## Part 2: order of evaluation on `runghc skyscrapers.hs`

1. All 309 lines are read as definitions. Nothing runs.
2. `main` is evaluated. Its first action is `run` on `puzzle5`.
3. `run` prints the header, then needs `printResult` to choose an equation, which needs to know if `solve puzzle5 (emptyGrid 5)` is `Nothing` or `Just`. This is the demand that starts the solver.
4. `solve` matches the clues and evaluates `go [] rowOpts`.
5. `go` matches `(opts : rest)`, forcing the first element of `rowOpts`, which runs `rowOptions 0`: `lineOptions`, `perms`, `matches`, `visible`, then `fitsGivens`.
6. `firstJust` asks for the first element of the comprehension: `tryRow` with row 0's first ordering.
7. `columnsOk` forces `colOpts` and runs `column`, `stillPossible`, `isPrefix`, `allTrue`.
8. Guard true: `go` one row deeper. Guard false: `Nothing`, and `firstJust` asks for the next ordering, which is computed only now.
9. When `go rows []` is reached, `Just rows` travels up through every `firstJust`.
10. `printResult` sees `Just`, prints the rows, compares with the published solution. Then `main` moves to the next `run`.

---

## Part 3: questions to expect

### About the algorithm

**Why row by row instead of cell by cell?**
The visibility clue is a property of a whole line. Cell by cell you could only check it when the line completes. Row by row, the row clues are satisfied by construction and only the columns need checking. It also makes the candidate set tiny: a row with clues 1 and 3 has 11 orderings instead of 120.

**Is the search complete? Could it miss a solution?**
No. Every solution is a sequence of rows, each from that row's allowed orderings, and `firstJust` tries every ordering in order. `columnsOk` only rejects a row when some column can no longer become any allowed column, and such a row cannot be in any solution. So nothing valid is ever pruned.

**Does it always terminate?**
Yes. Each row has finitely many orderings, the depth is at most n, and every recursive call has one fewer row to place.

**What is the worst case?**
A row with no clues has n! orderings, 720 for n = 6. Puzzle 42 has exactly that: row 4 has no clues and no given cells. It still solves in well under a second because the column checks reject most of those 720 quickly. A board with several such rows and weak column clues would be slow, but such a board would not have a unique solution, so it never appears as a real puzzle.

**What if there are several solutions?**
It returns the first one found, in the order `perms` generates. To return all, replace `firstJust` with a function that keeps every `Just`, or make `go` return a list of grids.

**What happens when there is no solution?**
`firstJust []` gives `Nothing` at some depth, that becomes a `Nothing` element in the caller's list, and so on up. At the top, `solve` returns `Nothing` and `printResult` prints "sem solucao". The impossible 4x4 in the tracer shows this in eleven steps.

**Where is the backtracking? I see no "undo".**
`rows ++ [p]` builds a new list. The caller's `rows` is untouched. When the deeper call returns `Nothing`, the caller simply continues its loop with its own unchanged `rows`. In the Go version, the same step needed the bitmap entries cleared and the cell reset. In Haskell there is nothing to undo because nothing was mutated.

**Why check columns after every row instead of only at the end?**
Pruning. Without it, the solver would build entire grids before noticing a column was broken from row 2. With it, a bad row is rejected immediately. Checking only at the end still gives correct answers, just slower.

**How are the given cells handled?**
As one more filter on the row candidates, in `rowOptions`. A candidate that disagrees with a given cell is never in the list, so the search never sees it. Columns need nothing extra, because every row already respects its givens.

**What are the optimizations?**
Two. Precomputing the allowed orderings per row and per column once, and rejecting a row as soon as some column can no longer be completed. Both are cheap to explain and together make 6x6 instant.

### About the Haskell

**Why is `perms [] = [[]]` and not `[]`?**
There is exactly one way to order nothing: the empty ordering. If the base case were `[]`, the comprehension in the recursive case would have no `rest` to choose and every call would return `[]`.

**Why `reverse p` for the back clue?**
Looking in from the back means seeing the last building first. Reversing the list and applying the same `visible` gives that view without writing a second visibility function.

**What does `\\` do?**
List difference, from `Data.List`. `[1,2,3] \\ [2]` is `[1,3]`. It removes the first occurrence of each element of the right list from the left list.

**Why `data Clues` and not a tuple or four arguments?**
A tuple `([Int],[Int],[Int],[Int])` would work. The constructor documents which list is which and lets `solve` pattern match on a name. This is the `data Forma = Retangulo Float Float` pattern from the slides.

**Why are `rowOpts` and `colOpts` in `where` and not inside `go`?**
A `where` binding of `solve` is evaluated once and shared. If they were computed inside `go`, every recursive call would recompute all the permutations. Haskell does not memoize function calls automatically.

**What is `Maybe` and why use it?**
A type with two shapes, `Nothing` and `Just x`, on the type classes slides. It is how a function says "I might not have an answer" without special values or exceptions. `solve` either has a grid or it does not.

**Is `Maybe` used as a monad here?**
No. It is used only as a type with two constructors that we pattern match on. The monad instance from the slides would let `do` notation chain `Maybe` values, but nothing here needs that. The only monad in use is `IO`, in `main`, `run`, `printResult` and `printRows`.

**Where does laziness matter?**
Two places. In `go`, the comprehension of attempts is never fully built; `firstJust` forces one element at a time and stops at the first `Just`. In `stillPossible`, `/= []` stops at the first matching option. Both would still be correct in a strict language, just slower.

**Why the guard with `otherwise`?**
`otherwise` is just the name `True`. Guards are tried top to bottom and the first true one wins. It reads as "else".

**What does `!!` cost?**
It walks the list, so it is linear in the index. For n at most 8 that is irrelevant. For a big grid you would use an array.

**Can the program crash?**
`left !! i` and friends assume all four clue lists and the givens grid have length n. If a list is shorter, `!!` fails at runtime. `perms` on a large list would take forever, not crash. Nothing else is partial.

**What is the type `[[[Int]]]` in `go`?**
A list of option lists, one per row still to place. Each option list is a list of orderings. Each ordering is a list of Int. Three levels.

**Why `return ()` in `printRows []`?**
Every equation of a function must have the same type, here `IO ()`. `return ()` is the IO action that does nothing and yields `()`. It is not a return statement.

**Could you write `go` with the list monad or `do` notation?**
Yes. `do p <- opts; ...` in the list monad tries every `p` and returns a list of all solutions. Taking the head gives the first. `firstJust` over `Maybe` does the same thing with the failure case visible in the types.

**How would you read the puzzle from a file?**
`readFile` gives an `IO String`. Split with `lines`, then `words`, then `read` each number, and build a `Clues`. `solve` does not change. The IO stays in `main`, the parsing and solving stay pure. The assignment explicitly allows input in the source, which is what the file does.

### Comparing with the Go version

**Same algorithm?**
Same search order for the same option ordering, same pruning idea. The Go version worked cell by cell with three bitmaps; this one works row by row with precomputed orderings. The Go version undid every placement on backtrack; this one never modifies anything, so there is nothing to undo.

**Which is faster?**
For 6x6 both are instant. Go with mutation is faster per step; Haskell with row candidates does far fewer steps.

**Where did the explicit stack go?**
It became the call stack. Each `go` call is one frame, holding its `rows` and which candidate it is on. The tracer's "recursion stack" panel shows exactly those frames.

---

## Numbers worth having in your head

| Fact | Value |
|---|---|
| Orderings of 1..6 | 720 |
| Orderings of 1..6 with left clue 1 | 120, the 6 must come first |
| Orderings of 1..6 with left clue 6 | 1, ascending |
| Orderings of 1..6 with left clue 2 | 274 |
| Row 0 of puzzle5, clues 1 and 3 | 11 |
| puzzle5: candidate rows tried, backtracks | 133, 12 |
| Nr. 3: candidate rows tried, backtracks | 201, 9 |
| Nr. 42: candidate rows tried, backtracks | 14,649, 32 |
| Nr. 43: candidate rows tried, backtracks | 345, 1 |
| All four boards, `runghc`, interpreted | about 2 s, mostly interpreter startup |
| One 6x6 with all clues, in ghci | 0.02 s |
