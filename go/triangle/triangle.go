// Package triangle determines the Kind of valid triangle
package triangle

import (
	"math"
)

// Kind is an enum of the type of Triangle
type Kind int

const (
	NaT Kind = iota // Invalid
	Equ             // Equilateral
	Iso             // Isosceles
	Sca             // Scalene
)

func any(nums []float64, pred func(float64) bool) bool {
	for _, n := range nums {
		if pred(n) {
			return true
		}
	}

	return false
}

// KindFromSides determines the Kind of triangle specified by the length of the triangle sides
func KindFromSides(a, b, c float64) (k Kind) {

	if any(
		[]float64{a, b, c},
		func(f float64) bool {
			return f <= 0 || math.IsNaN(f) || math.IsInf(f, 1)
		},
	) {
		return
	}

	if a+b < c || b+c < a || a+c < b {
		return
	}

	if a == b && b == c {
		return Equ
	}

	if a == b || b == c || a == c {
		return Iso
	}

	return Sca
}
