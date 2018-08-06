// Package leap contains the Exercism leap problem
package leap

// IsLeapYear determines if the year passed to it is a leap year
func IsLeapYear(y int) bool {
	switch {
	case y%400 == 0:
		return true
	case y%100 == 0:
		return false
	case y%4 == 0:
		return true
	default:
		return false
	}
}
