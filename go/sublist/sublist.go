package sublist

type Relation string

// Sublist determines if slice a is a sublist of b, equal, superlist, or unequal
func Sublist(a, b []int) Relation {
	if Equal(a, b) {
		return "equal"
	} else if len(a) > len(b) && isSubList(b, a) {
		return "superlist"
	} else if isSubList(a, b) {
		return "sublist"
	}

	return "unequal"
}

// Equal checks if two int slices are equal
func Equal(a, b []int) bool {
	if len(a) != len(b) {
		return false
	}
	for i, v := range a {
		if v != b[i] {
			return false
		}
	}
	return true
}

func isSubList(a, b []int) bool {
	l := len(a)
	for i := 0; i+l <= len(b); i++ {
		if Equal(a, b[i:l+i]) {
			return true
		}
	}

	return false
}
