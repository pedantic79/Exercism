package diffsquares

// SquareOfSums calculates the square of sum of 1 to n
func SquareOfSums(n int) int {
	return (n * n * (n + 1) * (n + 1)) / 4
}

// SumOfSquares calculates the sum of the squares of 1 to n
func SumOfSquares(n int) int {
	return (n * (n + 1) * (2*n + 1)) / 6
}

// Difference calculates the difference between SquareOfSums and SumOfSquares
func Difference(n int) int {
	return SquareOfSums(n) - SumOfSquares(n)
}
