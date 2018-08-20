package collatzconjecture

import "fmt"

// CollatzConjecture calculates the number of steps
func CollatzConjecture(n int) (int, error) {
	var steps int

	if n < 1 {
		return 0, fmt.Errorf("input less than %d < 1", n)
	}

	for n > 1 {
		if n%2 == 0 {
			n /= 2
		} else {
			n = 3*n + 1
		}
		steps++
	}

	return steps, nil
}
