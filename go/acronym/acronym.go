// Package acronym
package acronym

import (
	"strings"
	"unicode"
)

// Abbreviate should have a comment documenting it.
func Abbreviate(s string) string {
	var abbreviation strings.Builder
	words := strings.FieldsFunc(s, func(r rune) bool {
		return !unicode.IsLetter(r)
	})

	for _, word := range words {
		rn := []rune(word)[0]
		abbreviation.WriteRune(unicode.ToUpper(rn))
	}
	return abbreviation.String()
}
