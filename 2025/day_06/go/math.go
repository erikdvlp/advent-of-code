package main

func solve(matrix [][]int, operators []string) int {
	results := []int{}
	for i := range matrix {
		if operators[i] == "+" {
			results = append(results, sum(matrix[i]))
		} else {
			results = append(results, product(matrix[i]))
		}
	}
	return sum(results)
}

func sum(numbers []int) int {
	sum := 0
	for _, n := range numbers {
		sum += n
	}
	return sum
}

func product(numbers []int) int {
	product := 1
	for _, n := range numbers {
		product *= n
	}
	return product
}
