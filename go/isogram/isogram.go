package isogram

import (
	"unicode"
)

//IsIsogram determines if the input string is an isogram
func IsIsogram(input string) bool {
	runeInput := []rune(input)

	for idx, ch := range runeInput {
		for jdx := idx + 1; jdx < len(runeInput); jdx++ {
			if unicode.IsLetter(ch) &&
				unicode.ToLower(ch) == unicode.ToLower(runeInput[jdx]) {

				return false
			}
		}
	}
	return true
}
