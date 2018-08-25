package letter

import (
	"sync"
)

// ConcurrentFrequency calculates the rune frequency concurrently
func ConcurrentFrequency(strs []string) FreqMap {
	l := len(strs)

	if l == 0 {
		return FreqMap{}
	}

	if l == 1 {
		return Frequency(strs[0])
	}

	ch := make(chan FreqMap)
	f := func(ss []string) {
		ch <- ConcurrentFrequency(ss)
	}

	go f(strs[:l/2])
	go f(strs[l/2:])

	m := <-ch
	for k, v := range <-ch {
		m[k] += v
	}

	return m
}

func ConcurrentFrequency2(ss []string) FreqMap {
	ch := make(chan FreqMap)
	var wg sync.WaitGroup

	var helper func([]string)
	helper = func(strs []string) {
		defer wg.Done()

		l := len(strs)
		if l == 1 {
			ch <- Frequency(strs[0])
		}
		if l > 1 {
			wg.Add(2)
			go helper(strs[:l/2])
			go helper(strs[l/2:])
		}
	}

	go func() {
		wg.Add(1)
		go helper(ss)
		wg.Wait()
		close(ch)
	}()

	m := FreqMap{}
	// Using the following seems to loop forever
	// for fm, ok := <-ch; ok; {
	// 	for k, v := range fm {
	// 		m[k] += v
	// 	}
	// }
	for {
		if fm, ok := <-ch; ok {
			for k, v := range fm {
				m[k] += v
			}
		} else {
			break
		}
	}
	return m
}
