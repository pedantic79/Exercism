object Pangram {
    fun isPangram(s: String) = s.toLowerCase().replace(Regex("[^a-z]"), "").toSet().size == 26
}