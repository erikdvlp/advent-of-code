package main

import (
	"strconv"
)

// Part 1: Count dial position zero at the end of each rotation
func countMethodOne(steps []string) int {
	dial := 50
	zeroes := 0

	for _, step := range steps {
		dir := step[0]
		clicks, _ := strconv.Atoi(step[1:])

		if dir == 'R' {
			dial += clicks
		} else {
			dial -= clicks
		}

		dial = dial % 100
		if dial < 0 {
			dial += 100
		}

		if dial == 0 {
			zeroes++
		}
	}
	return zeroes
}

// Part 2: Count dial position zero after each click
func countMethodTwo(steps []string) int {
	dial := 50
	zeroes := 0

	for _, step := range steps {
		dir := step[0]
		clicks, _ := strconv.Atoi(step[1:])

		for range clicks {
			if dir == 'R' {
				dial++
			} else {
				dial--
			}

			dial = dial % 100
			if dial < 0 {
				dial += 100
			}

			if dial == 0 {
				zeroes++
			}
		}
	}
	return zeroes
}
