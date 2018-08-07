// Package bob should have a package comment that summarizes what it's about.
// https://golang.org/doc/effective_go.html#commentary
package bob

import (
	"strings"
)

// Hey should have a comment documenting it.
func Hey(remark string) string {
	remark = strings.TrimSpace(remark)
	switch {
	case remark == "":
		return "Fine. Be that way!"
	case isYell(remark):
		if isQuestion(remark) {
			return "Calm down, I know what I'm doing!"
		}
		return "Whoa, chill out!"
	case isQuestion(remark):
		return "Sure."
	default:
		return "Whatever."
	}
}

func isQuestion(s string) bool {
	return strings.HasSuffix(s, "?")
}

func isYell(s string) bool {
	up := strings.ToUpper(s)

	return up == s && up != strings.ToLower(s)
}
