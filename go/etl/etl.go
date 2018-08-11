package etl

import (
	"strings"
)

// Transform turns a map of scores and slice of letters
// to a map of letters to scores
func Transform(input map[int][]string) map[string]int {
	output := make(map[string]int)
	for score, letters := range input {
		for _, letter := range letters {
			output[strings.ToLower(letter)] = score
		}
	}
	return output
}
