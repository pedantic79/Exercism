object WordCount {
    private val nonWordChars = Regex("[^'a-zA-Z0-9]+")
    fun phrase(input: String): Map<String, Int> =
            input.split(nonWordChars)
                    .asSequence()
                    .filter { it != "" }
                    .groupingBy { it.toLowerCase().removeSurrounding("'") }
                    .eachCount()
}
