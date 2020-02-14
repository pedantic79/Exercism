package atbash

import (
	"strings"
	"unicode"
)

func Atbash(input string) string {
	var sb strings.Builder
	count := 0
	for _, c := range input {
		f, ok := flip(c)
		if ok {
			if count != 0 && count%5 == 0 {
				sb.WriteRune(' ')
			}

			sb.WriteRune(f)
			count++
		}
	}

	return sb.String()
}

func flip(c rune) (rune, bool) {
	if isAscii(c) && unicode.IsLetter(c) {
		return 'a' + 'z' - unicode.ToLower(c), true
	} else if isAscii(c) && unicode.IsNumber(c) {
		return c, true
	} else {
		return '*', false
	}
}

func isAscii(c rune) bool {
	return c < 127
}
