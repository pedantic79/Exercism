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
	return Clock{mins: (hour*60 + minute) % day}.normalize()
}

// String returns the string representation of Clock
func (clock Clock) String() string {
	hours := clock.mins / 60
	if hours == 24 {
		hours = 0
	}
	return fmt.Sprintf("%02d:%02d", hours, clock.mins%60)
}

// Add minutes to the clock
func (clock Clock) Add(minutes int) Clock {
	return Clock{mins: clock.mins + minutes}.normalize()
}

// Subtract minutes from the clock
func (clock Clock) Subtract(minutes int) Clock {
	return Clock{mins: clock.mins - minutes}.normalize()
}

func abs(num int) int {
	if num < 0 {
		return num * -1
	}
	return num
}

func (clock Clock) normalize() Clock {
	if clock.mins <= 0 {
		clock.mins += (abs(clock.mins)/day + 1) * day
	}
	if clock.mins > day {
		clock.mins %= day
	}

	return Clock{mins: clock.mins}
}
