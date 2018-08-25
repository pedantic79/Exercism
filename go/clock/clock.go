package clock

import (
	"fmt"
)

// Clock is a simple 24hour clock
type Clock struct {
	mins int
}

const day = 1440

// New creates a new Clock struct
func New(hour, minute int) Clock {
	return Clock{mins: hour*60 + minute}.normalize()
}

// String returns the string representation of Clock
func (clock Clock) String() string {
	return fmt.Sprintf("%02d:%02d", clock.mins/60, clock.mins%60)
}

// Add minutes to the clock
func (clock Clock) Add(minutes int) Clock {
	return Clock{mins: clock.mins + minutes}.normalize()
}

// Subtract minutes from the clock
func (clock Clock) Subtract(minutes int) Clock {
	return Clock{mins: clock.mins - minutes}.normalize()
}

func (clock Clock) normalize() Clock {
	clock.mins = clock.mins%day + day
	clock.mins %= day

	return Clock{mins: clock.mins}
}
