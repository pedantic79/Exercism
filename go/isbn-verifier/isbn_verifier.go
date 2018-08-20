package isbn

import (
	"unicode"
)

// IsValidISBN calculates if your string is a valid ISBN-10 value
func IsValidISBN(isbNum string) bool {
	var length, sum int
	runeStr := []rune(isbNum)

	for i := len(runeStr) - 1; i >= 0; i-- {
		switch {
		case unicode.IsDigit(runeStr[i]):
			length++
			n := int(runeStr[i]) - int(rune('0'))
			sum += length * n
		case runeStr[i] == '-':
		case runeStr[i] == 'X':
			if length != 0 {
				return false
			}
			length++
			sum += 10
		default:
			return false
		}
	}

	return length == 10 && sum%11 == 0
}
