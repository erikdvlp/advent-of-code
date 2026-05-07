package main

import (
	"fmt"
	"os"
	"strings"
)

func main() {
	lines := readLines()

	result1 := countMethodOne(lines)
	fmt.Println("Part 1 answer:", result1)

	result2 := countMethodTwo(lines)
	fmt.Println("Part 2 answer:", result2)
}

func readLines() []string {
	data, _ := os.ReadFile("../input.txt")
	lines := strings.Split(strings.TrimSpace(string(data)), "\n")
	return lines
}
