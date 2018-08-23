data class Squares(val n: Long) {
    fun squareOfSum() = (n * n * (n + 1) * (n + 1)) / 4
    fun sumOfSquares() = (n * (n + 1) * (2*n + 1)) / 6
    fun difference() = squareOfSum() - sumOfSquares()
}
