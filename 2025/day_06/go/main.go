package main

import (
	"fmt"
	"os"
	"strings"
)

func main() {
	lines := readLines()
	matrix, operators := parse(lines)

	result1 := solve(matrix, operators)
	fmt.Println("Part 1 result:", result1)
}

func readLines() []string {
	data, _ := os.ReadFile("../input.txt")
	lines := strings.Split(strings.TrimSpace(string(data)), "\n")
	return lines
}
