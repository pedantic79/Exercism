object WordCount {
    fun phrase(input: String): Map<String, Int> =
            input.replace(Regex("[^'a-zA-Z0-9]+"), " ")
                    .split(" ")
                    .fold(mutableMapOf()) { acc, word ->
                        if (word != "") {
                            val key = word.toLowerCase().removeSurrounding("'")
                            acc.compute(key) { _, v -> (v ?: 0) + 1 }
                        }
                        acc
                    }

}