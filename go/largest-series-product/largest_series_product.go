package lsproduct

import (
	"errors"
	"math"
)

func LargestSeriesProduct(input string, span int) (int, error) {
	digits, err := digits(input)

	if err != nil {
		return 0, err
	}

	if span > len(input) || span < 0 {
		return 0, errors.New("invalid length")
	}

	max := math.MinInt64
	for i := 0; i+span <= len(input); i++ {
		total := 1
		for _, digit := range digits[i : i+span] {
			total *= digit
		}
		if total > max {
			max = total
		}
	}

	return max, nil
}

func digits(input string) ([]int, error) {
	output := make([]int, len(input))

	for i, char := range input {
		n := int(char)
		if n >= '0' && n <= '9' {
			output[i] = n - '0'
		} else {
			return nil, errors.New("invalid character")
		}
	}

	return output, nil
}
