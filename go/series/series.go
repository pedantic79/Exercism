package series

func All(n int, s string) []string {
	output := []string{}

	for i := 0; i+n <= len(s); i++ {
		output = append(output, s[i:i+n])
	}
	return output
}

func UnsafeFirst(n int, s string) string {
	return s[:n]
}

func First(n int, s string) (first string, ok bool) {
	if n > len(s) {
		return "", false
	}

	return s[:n], true
}
