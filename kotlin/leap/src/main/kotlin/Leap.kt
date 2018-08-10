class Year(private val year : Int) {
    val isLeap: Boolean by lazy {
        divisibleBy(4) && (!divisibleBy(100) || divisibleBy(400))
    }

    private fun divisibleBy(divisor: Int) = year % divisor == 0
}
