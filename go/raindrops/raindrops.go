package raindrops

import (
	"fmt"
	"strings"
)

// Convert integer to PlingPlangPlong
func Convert(number int) string {
	var builder strings.Builder

	if divisibleBy(number, 3) {
		builder.WriteString("Pling")
	}
	if divisibleBy(number, 5) {
		builder.WriteString("Plang")
	}
	if divisibleBy(number, 7) {
		builder.WriteString("Plong")
	}

	if builder.Len() == 0 {
		return fmt.Sprintf("%d", number)
	}

	return builder.String()
}

func divisibleBy(number, divisor int) bool {
	return number%divisor == 0
}
