package luhn

import (
	"unicode"
)

func Valid(luhnStr string) bool {
	luhnRune := []rune(luhnStr)

	var pos, sum int
	for i := len(luhnRune) - 1; i >= 0; i-- {
		if unicode.IsSpace(luhnRune[i]) {
			continue
		}

		if !unicode.IsDigit(luhnRune[i]) {
			return false
		}

		numeric := int(luhnRune[i]) - '0'
		if pos%2 == 0 {
			sum += numeric
		} else {
			sum += numeric * 2
			if numeric > 4 {
				sum -= 9
			}
		}
		pos++
	}

	if pos < 2 {
		return false
	}
	return sum%10 == 0
}
