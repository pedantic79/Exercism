import java.util.*

class Robot {
    companion object {
        private val random = Random()
    }

    var name = generateName()
        private set

    private fun generateName(): String {
        val a = random.nextInt(26)
        val b = random.nextInt(26)
        val n = random.nextInt(999)

        return "%c%c%03d".format('A' + a, 'A' + b, n)
    }

    fun reset() {
        name = generateName()
    }

}
