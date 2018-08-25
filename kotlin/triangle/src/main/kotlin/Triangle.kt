
class Triangle<T>(val side1: T, val side2: T, val side3: T)
    where
        T: Number,
        T: Comparable<T> {

    init {
        val sorted = arrayListOf(side1, side2, side3).sorted()

        // Is there a way to implement this?
        // Either an Addable or Monoid interface
        val valid = when (sorted[0]) {
            is Long   -> sorted[0].toLong()   + sorted[1].toLong()   > sorted[2].toLong()
            is Int    -> sorted[0].toInt()    + sorted[1].toInt()    > sorted[2].toInt()
            is Short  -> sorted[0].toShort()  + sorted[1].toShort()  > sorted[2].toShort()
            is Byte   -> sorted[0].toByte()   + sorted[1].toByte()   > sorted[2].toByte()
            is Double -> sorted[0].toDouble() + sorted[1].toDouble() > sorted[2].toDouble()
            is Float  -> sorted[0].toFloat()  + sorted[1].toFloat()  > sorted[2].toFloat()
            else      -> throw RuntimeException("Unknown numeric type")
        }

        require(valid)
    }

    private val equalSidesCount = arrayOf(
            equalToInt(side1, side2),
            equalToInt(side1, side3),
            equalToInt(side2, side3)).sum()


    val isEquilateral = equalSidesCount == 3

    val isIsosceles = equalSidesCount >= 1

    val isScalene = equalSidesCount == 0
}

fun<T: Comparable<T>> equalToInt(a: T, b: T): Int = if (a == b) 1 else 0
