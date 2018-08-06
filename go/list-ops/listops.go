//Package listops contains the list operation for Exercism
package listops

//IntList type is a simple wrapper around int slice
type IntList []int
type binFunc func(int, int) int
type predFunc func(int) bool
type unaryFunc func(int) int

//Length returns the length of elements in the list
func (list IntList) Length() int {
	return len(list)
}

//Foldr reduces from the end of the list to the begining of the list
//by applying the binFunc to the accumulator
func (list IntList) Foldr(fn binFunc, accumulator int) int {
	acc := accumulator
	for i := list.Length() - 1; i >= 0; i-- {
		acc = fn(list[i], acc)
	}
	return acc
}

//Foldl reduces from the begining of the list to the end of the list
//by applying the binFunc to the accumulator
func (list IntList) Foldl(fn binFunc, accumulator int) int {
	acc := accumulator
	for _, v := range list {
		acc = fn(acc, v)
	}
	return acc
}

//Map applies the fn unaryFunc to every element in the list
func (list IntList) Map(fn unaryFunc) IntList {
	for i := range list {
		list[i] = fn(list[i])
	}
	return list
}

//Filter only includes elements from the list where pred is true on the element
func (list IntList) Filter(pred predFunc) IntList {
	l := IntList([]int{})

	for _, v := range list {
		if pred(v) {
			l = l.Append([]int{v})
		}
	}
	return l
}

//Reverse reverses the order of the list
func (list IntList) Reverse() IntList {
	for i := 0; i < int(list.Length()/2); i++ {
		list[i], list[list.Length()-1-i] = list[list.Length()-1-i], list[i]
	}
	return list
}

//Append appends an array of ints to the list
func (list IntList) Append(l []int) IntList {
	return append(list, l...)
}

//Concat combines multiple IntLists to the IntList
func (list IntList) Concat(l []IntList) IntList {
	for _, v := range l {
		list = list.Append(v)
	}
	return list
}
