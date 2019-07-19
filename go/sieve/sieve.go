package sieve

func Sieve(limit int) []int {
	notPrime := make([]bool, limit-1)
	var primes []int

	for idx := 2; idx <= limit; idx++ {
		if !notPrime[idx-2] {
			primes = append(primes, idx)
			for n := idx * 2; n <= limit; n = n + idx {
				notPrime[n-2] = true
			}
		}
	}

	return primes
}
