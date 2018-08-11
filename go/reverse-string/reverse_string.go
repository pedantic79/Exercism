package reverse

// String reverses the input string
func String(input string) string {
	runeInput := []rune(input)
	length := len(runeInput) / 2
	for idx := 0; idx < length; idx++ {
		revIdx := len(runeInput) - idx - 1
		runeInput[idx], runeInput[revIdx] = runeInput[revIdx], runeInput[idx]
	}

	return string(runeInput)
}
