package strain

type Ints []int
type Lists [][]int
type Strings []string

func filter(splitable Ints, pred func(int) bool) Ints {
	var output []int

	for _, r := range splitable {
		if pred(r) {
			output = append(output, r)
		}
	}

	return Ints(output)
}

func (s Ints) Keep(pred func(int) bool) Ints {
	return filter(s, pred)
}

func (s Ints) Discard(pred func(int) bool) Ints {
	return filter(s, func(i int) bool { return !pred(i) })
}

func (s Lists) Keep(pred func([]int) bool) Lists {
	var output [][]int

	for _, r := range s {
		if pred(r) {
			output = append(output, r)
		}
	}

	return Lists(output)
}

func (s Strings) Keep(pred func(string) bool) Strings {
	var output []string

	for _, r := range s {
		if pred(r) {
			output = append(output, r)
		}
	}

	return Strings(output)
}
