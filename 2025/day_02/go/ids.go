package main

import (
	"strconv"
	"strings"
)

type Part int

const (
	Part1 Part = iota
	Part2
)

func sumInvalidIds(ranges []string, part Part) int {
	invalid := []int{}

	for _, r := range ranges {
		lower, upper := getRange(r)
		for i := lower; i <= upper; i++ {
			var doesRepeat func(string) bool
			if part == Part1 {
				doesRepeat = doesRepeatTwice
			} else {
				doesRepeat = doesRepeatTwiceOrMore
			}

			if doesRepeat(strconv.Itoa(i)) {
				invalid = append(invalid, i)
			}
		}
	}

	sum := 0
	for _, n := range invalid {
		sum += n
	}
	return sum
}

func getRange(s string) (int, int) {
	bounds := strings.Split(s, "-")
	lower, _ := strconv.Atoi(bounds[0])
	upper, _ := strconv.Atoi(bounds[1])
	return lower, upper
}

// Part 1: ID repeats exactly twice
func doesRepeatTwice(s string) bool {
	n := len(s)

	if n%2 != 0 {
		return false
	}

	return s[:n/2] == s[n/2:]
}

// Part 2: ID repeats twice or more
func doesRepeatTwiceOrMore(s string) bool {
	n := len(s)

	for slice := 1; slice <= n/2; slice++ {
		if n%slice == 0 && strings.Repeat(s[:slice], n/slice) == s {
			return true
		}
	}

	return false
}
