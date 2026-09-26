package main

import (
	"fmt"
)

func main() {
	fmt.Println("hello world!")
}

type Coordinate struct {
	x, y int
}

// stack-based
func solveSudoku(board [][]byte) {
	mutableCords := []Coordinate{}
	rowsBitMap := [9][10]bool{}
	columnsBitMap := [9][10]bool{}
	squaresBitMap := [9][10]bool{}

	for i := range board {
		for j := range board[i] {
			val := board[i][j]
			if val == '.' {
				mutableCords = append(mutableCords, Coordinate{x: i, y: j})
				continue
			}

			box := (i/3)*3 + (j / 3)

			rowsBitMap[i][val-'0'] = true
			columnsBitMap[j][val-'0'] = true
			squaresBitMap[box][val-'0'] = true

		}
	}

	stack := []Coordinate{}
	pos := 0
	start := byte('1')

	for {
		if pos == len(mutableCords) {
			return
		}

		placed := false
		coord := mutableCords[pos]
		for i := start; i <= '9'; i++ {
			valid := !rowsBitMap[coord.x][i-'0'] &&
				!columnsBitMap[coord.y][i-'0'] &&
				!squaresBitMap[(coord.x/3)*3+(coord.y/3)][i-'0']

			if !valid {
				continue
			}

			board[coord.x][coord.y] = i
			stack = append(stack, coord)
			placed = true
			rowsBitMap[coord.x][i-'0'] = true
			columnsBitMap[coord.y][i-'0'] = true
			squaresBitMap[(coord.x/3)*3+(coord.y/3)][i-'0'] = true

			break
		}

		if placed {
			pos += 1
			start = '1'
			continue
		}

		if len(stack) == 0 {
			return
		}

		top := stack[len(stack)-1]
		stack = stack[:len(stack)-1]

		val := board[top.x][top.y]
		rowsBitMap[top.x][val-'0'] = false
		columnsBitMap[top.y][val-'0'] = false
		squaresBitMap[(top.x/3)*3+(top.y/3)][val-'0'] = false

		start = val + 1
		board[top.x][top.y] = '.'
		pos -= 1
	}
}

// recursive
func solveSudokuRec(board [][]byte) bool {
	mutableCords := []Coordinate{}
	rowsBitMap := [9][10]bool{}
	columnsBitMap := [9][10]bool{}
	squaresBitMap := [9][10]bool{}

	for i := range board {
		for j := range board[i] {
			val := board[i][j]
			if val == '.' {
				mutableCords = append(mutableCords, Coordinate{x: i, y: j})
				continue
			}

			box := (i/3)*3 + (j / 3)

			rowsBitMap[i][val-'0'] = true
			columnsBitMap[j][val-'0'] = true
			squaresBitMap[box][val-'0'] = true
		}
	}

	// declared before it is assigned so the body can call itself
	var solve func(idx int) bool
	solve = func(idx int) bool {
		if idx == len(mutableCords) {
			return true
		}

		coord := mutableCords[idx]
		box := (coord.x/3)*3 + (coord.y / 3)

		for i := byte('1'); i <= '9'; i++ {
			valid := !rowsBitMap[coord.x][i-'0'] &&
				!columnsBitMap[coord.y][i-'0'] &&
				!squaresBitMap[box][i-'0']

			if !valid {
				continue
			}

			board[coord.x][coord.y] = i
			rowsBitMap[coord.x][i-'0'] = true
			columnsBitMap[coord.y][i-'0'] = true
			squaresBitMap[box][i-'0'] = true

			if solve(idx + 1) {
				return true
			}

			// the search below this choice failed: undo it and try the next digit
			rowsBitMap[coord.x][i-'0'] = false
			columnsBitMap[coord.y][i-'0'] = false
			squaresBitMap[box][i-'0'] = false
			board[coord.x][coord.y] = '.'
		}

		return false
	}

	return solve(0)
}

func checkConstraints(board [][]byte, x, y int) bool {
	return checkRowConstraint(board, x) &&
		checkColumnConstraint(board, y) &&
		checkSquareConstraint(board, x, y)

}

func checkRowConstraint(board [][]byte, row int) bool {
	seen := make(map[byte]bool, 9)
	for _, val := range board[row] {
		if val == '.' {
			continue
		}

		if seen[val] {
			return false
		}

		seen[val] = true
	}

	return true
}

func checkColumnConstraint(board [][]byte, column int) bool {
	seen := make(map[byte]bool, 9)

	for _, row := range board {
		val := row[column]

		if val == '.' {
			continue
		}

		if seen[val] {
			return false
		}

		seen[val] = true
	}

	return true
}

func checkSquareConstraint(board [][]byte, x, y int) bool {
	seen := make(map[byte]bool, 9)

	xCord := (x / 3) * 3
	yCord := (y / 3) * 3

	for i := xCord; i < xCord+3; i++ {
		for j := yCord; j < yCord+3; j++ {
			val := board[i][j]

			if val == '.' {
				continue
			}

			if seen[val] {
				return false
			}

			seen[val] = true
		}
	}

	return true
}
