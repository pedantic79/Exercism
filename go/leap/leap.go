// Package leap contains the Exercism leap problem
package leap

// IsLeapYear determines if the year passed to it is a leap year
func IsLeapYear(y int) bool {
	return y%4 == 0 && (y%100 != 0 || y%400 == 0)
}
