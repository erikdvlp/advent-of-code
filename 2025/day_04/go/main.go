package main

import (
	"fmt"
	"os"
	"strings"
)

func main() {
	lines := readLines()
	grid := createGrid(lines)

	result1 := len(accessibleRolls(grid))
	fmt.Println("Part 1 result:", result1)

	result2 := removableRolls(grid)
	fmt.Println("Part 2 result:", result2)
}

func readLines() []string {
	data, _ := os.ReadFile("../input.txt")
	lines := strings.Split(strings.TrimSpace(string(data)), "\n")
	return lines
}
