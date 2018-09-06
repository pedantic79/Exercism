import java.util.*
import kotlin.collections.HashSet

class Robot {
    companion object {
        private val random = Random()
        private val nameCache = HashSet<String>()
    }

    var name = generateUniqueName()
        private set

    private fun generateName(): String {
        val a = random.nextInt(26)
        val b = random.nextInt(26)
        val n = random.nextInt(1000)

        return "%c%c%03d".format('A' + a, 'A' + b, n)
    }

    private fun generateUniqueName(): String {
        var name = generateName()
        while (nameCache.contains(name)) {
            name = generateName()
        }

        nameCache.add(name)
        return name
    }

    fun reset() {
        nameCache.remove(name)
        name = generateUniqueName()
    }

}
