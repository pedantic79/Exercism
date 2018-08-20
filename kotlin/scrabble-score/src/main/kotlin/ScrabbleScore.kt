package ScrabbleScore

val letterScore = mapOf("aeioulnrst" to 1,
                        "dg" to 2,
                        "bcmp" to 3,
                        "fhvwy" to 4,
                        "k" to 5,
                        "jx" to 8,
                        "qz" to 10
                        )

fun scoreWord(word: String): Int =
    word.map({ c -> letterLookup(c) }).sum()

fun letterLookup(letter: Char): Int {
    for ((k, v) in letterScore) {
        if (k.contains(letter, true)) {
            return v
        }
    }

    return 0
}
