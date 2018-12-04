data class Series(val numberString: String) {
    init {
        require(numberString.all { c -> c in '0'..'9' })
    }

    fun getLargestProduct(size: Int): Int {
        require(size <= numberString.length)
        return if (size == 0) {
            1
        } else {
            numberList().windowed(size).map { w -> w.reduce { acc, i -> acc * i } }
                    .max() ?: 1
        }
    }


    private fun numberList(): List<Int> =
            numberString.map { c ->
                c.toInt() - '0'.toInt()
            }

}
