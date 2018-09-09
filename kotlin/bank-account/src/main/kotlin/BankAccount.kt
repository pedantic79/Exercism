import java.util.concurrent.locks.ReentrantReadWriteLock
import kotlin.concurrent.read
import kotlin.concurrent.write

class BankAccount {
    private val lock = ReentrantReadWriteLock()

    var balance: Int? = 0
        get() = lock.read {
            checkNotNull(field)
            field
        }
        private set


    fun adjustBalance(amount: Int) {
        lock.write {
            checkNotNull(balance)
            balance = balance?.let { it + amount }
        }
    }

    fun close() {
        lock.write {
            balance = null
        }
    }
}