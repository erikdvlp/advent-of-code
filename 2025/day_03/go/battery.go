package main

import "strconv"

func totalJoltage(banks []string, n int) int {
	sum := 0

	for _, bank := range banks {
		joltage := highestJoltage(bank, n)
		sum += joltage
	}

	return sum
}

func highestJoltage(bank string, n int) int {
	result := 0
	start := 0

	// Pick batteries one at a time and track how many remain
	for remaining := n; remaining > 0; remaining-- {
		// Calculate the rightmost position we can consider for this pick
		// so we still have enough batteries left to pick n batteries total
		end := len(bank) - remaining + 1

		highestBattery := -1
		highestBatteryIndex := -1

		// Find highest joltage battery in range
		for i := start; i < end; i++ {
			battery, _ := strconv.Atoi(string(bank[i]))
			if battery > highestBattery {
				highestBattery = battery
				highestBatteryIndex = i
			}
		}

		// Append battery to result
		result = result*10 + highestBattery

		// Step forward
		start = highestBatteryIndex + 1
	}

	return result
}
