// Package gigasecond contains AddGigasecond for the exercism/go/gigasecond
// problem.
package gigasecond

import (
	"time"
)

// AddGigasecond adds 1,000,000,000 to the time
func AddGigasecond(t time.Time) time.Time {
	gigasec, _ := time.ParseDuration("1000000000s")
	return t.Add(gigasec)
}
