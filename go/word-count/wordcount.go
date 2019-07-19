package wordcount

import (
	"strings"
	"unicode"
)

type Frequency map[string]int

func WordCount(words string) Frequency {
	count := Frequency{}

	for _, word := range strings.FieldsFunc(words, split) {
		word = strings.ToLower(word)
		word = strings.Trim(word, "\n")
		word = strings.Trim(word, "'")
		count[word]++
	}
	return count
}

func split(r rune) bool {
	return !(unicode.IsLetter(r) || unicode.IsNumber(r) || r == '\'')
}
