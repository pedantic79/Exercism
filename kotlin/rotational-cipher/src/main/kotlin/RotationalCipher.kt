data class RotationalCipher(val displacement: Int) {

    fun encode(text: String): String =
        text.map { it.move() }.joinToString("")

    private fun Char.move(): Char =
        if (this.isLetter()) {
            val a = if (this.isLowerCase()) {
                'a'
            } else {
                'A'
            }
            (((this - a + displacement) % 26) + a.toInt()).toChar()
        } else {
            this
        }
}

