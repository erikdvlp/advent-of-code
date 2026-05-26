package main

import (
	"strconv"
	"strings"
)

func parse(input []string) (matrix [][]int, operators []string) {
	matrix = [][]int{}
	for i := 0; i < len(input)-1; i++ {
		elements := strings.Fields(input[i])
		numbers := []int{}
		for _, e := range elements {
			n, _ := strconv.Atoi(e)
			numbers = append(numbers, n)
		}
		matrix = append(matrix, numbers)
	}
	matrix = transpose(matrix)

	operators = strings.Fields(input[len(input)-1])

	return
}

func transpose(matrix [][]int) [][]int {
	rows := len(matrix)
	cols := len(matrix[0])

	result := make([][]int, cols)
	for i := range result {
		result[i] = make([]int, rows)
	}

	for i := range matrix {
		for j := range matrix[i] {
			result[j][i] = matrix[i][j]
		}
	}

	return result
}
