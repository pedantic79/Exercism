package letter

// ConcurrentFrequency calculates the rune frequency concurrently
func ConcurrentFrequency(strs []string) FreqMap {
	ch := make(chan FreqMap)

	for _, s := range strs {
		go func(input string) {
			ch <- Frequency(input)
		}(s)
	}

	m := FreqMap{}
	for range strs {
		for k, v := range <-ch {
			m[k] += v
		}
	}
	return m
}
