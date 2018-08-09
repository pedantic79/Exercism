package space

import (
	"fmt"
)

//Planet type is just a strings
type Planet string

func (plnt *Planet) getRotationalFactor() (factor float64, err error) {
	switch *plnt {
	case "Earth":
		factor = 1
	case "Mercury":
		factor = 0.2408467
	case "Venus":
		factor = 0.61519726
	case "Mars":
		factor = 1.8808158
	case "Jupiter":
		factor = 11.862615
	case "Saturn":
		factor = 29.447498
	case "Uranus":
		factor = 84.016846
	case "Neptune":
		factor = 164.79132
	default:
		err = fmt.Errorf("Unknown planet %s", *plnt)
	}
	return
}

// Age calculates the relative age on another planet
func Age(seconds float64, planet Planet) float64 {
	const earthYear float64 = 31557600

	planetFactor, err := planet.getRotationalFactor()
	if err == nil {
		return seconds / earthYear / planetFactor
	}
	return 0
}
