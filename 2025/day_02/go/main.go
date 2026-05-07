package main

import (
	"fmt"
	"os"
	"strings"
)

func main() {
	lines := readLines()

	result1 := sumInvalidIds(lines, Part1)
	fmt.Println("Part 1 result:", result1)

	result2 := sumInvalidIds(lines, Part2)
	fmt.Println("Part 2 result:", result2)
}

func readLines() []string {
	data, _ := os.ReadFile("../input.txt")
	lines := strings.Split(strings.TrimSpace(string(data)), ",")
	return lines
}
