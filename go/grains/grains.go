package grains

import "fmt"

// Square counts the number of grains on each square
func Square(num int) (uint64, error) {
	if num < 1 || num > 64 {
		return 0, fmt.Errorf("invalid input (%d), must be 1 to 64", num)
	}

	return 1 << (uint(num) - 1), nil
}

// Total is 1 less than the next square because of math
func Total() uint64 {
	return (1 << 64) - 1
}
