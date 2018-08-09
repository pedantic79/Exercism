package accumulate

func Accumulate(input []string, fn func(string) string) (output []string) {
	for _, value := range input {
		output = append(output, fn(value))
	}

	return
}
