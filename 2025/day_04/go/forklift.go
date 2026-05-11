package main

func removableRolls(grid [][]rune) int {
	removable := 0

	for {
		accessible := accessibleRolls(grid)
		if len(accessible) == 0 {
			break
		}
		for _, roll := range accessible {
			x, y := roll[0], roll[1]
			grid[y][x] = '.'
			removable++
		}
	}

	return removable
}

func accessibleRolls(grid [][]rune) [][]int {
	rolls := [][]int{}

	for y := range grid {
		for x := 0; x < len(grid[0]); x++ {
			if grid[y][x] == '@' && adjacentRolls(grid, x, y) < 4 {
				rolls = append(rolls, []int{x, y})
			}
		}
	}

	return rolls
}

func adjacentRolls(grid [][]rune, x, y int) int {
	count := 0

	adjacent := [][]int{
		{x - 1, y - 1},
		{x - 1, y + 1},
		{x - 1, y},
		{x + 1, y - 1},
		{x + 1, y + 1},
		{x + 1, y},
		{x, y - 1},
		{x, y + 1},
	}

	for _, coords := range adjacent {
		a, b := coords[0], coords[1]
		if b >= 0 && b < len(grid) && a >= 0 && a < len(grid[0]) {
			if grid[b][a] == '@' {
				count++
			}
		}
	}

	return count
}

func createGrid(s []string) [][]rune {
	grid := make([][]rune, len(s))
	for i := range grid {
		grid[i] = []rune(s[i])
	}
	return grid
}
