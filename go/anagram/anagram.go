package anagram

import (
	"sort"
	"strings"
)

func Detect(word string, possibleAnagrams []string) []string {
	var output []string

	wordLowerCase, wordFreq := normalize(word)
	for _, possible := range possibleAnagrams {
		possibleLowerCase, freq := normalize(possible)

		if wordLowerCase != possibleLowerCase && wordFreq == freq {
			output = append(output, possible)
		}
	}

	return output
}

func normalize(word string) (string, string) {
	lc := strings.ToLower(word)

	r := []rune(lc)
	sort.Slice(r, func(i, j int) bool { return r[i] < r[j] })

	return lc, string(r)
}
