package pangram

import "strings"

func IsPangram(s string) bool {
	s = strings.ToLower(s)
	count := map[rune]bool{}

	for _, c := range s {
		if c >= 'a' && c <= 'z' {
			count[c] = true
		}
	}

	return len(count) == 26
}
