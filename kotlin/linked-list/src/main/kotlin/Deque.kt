class Deque<T> {
    private data class Node<T>(val value: T, var prev: Node<T>?, var next: Node<T>?)

    private var head: Node<T>? = null

    fun push(value: T) {
        if (head == null) {
            head = Node(value = value, next = null, prev = null)
            head?.next = head
            head?.prev = head
        } else {
            val node = Node(value = value, next = head, prev = head?.prev)
            head?.prev?.next = node
            head?.prev = node
            head = node
        }
    }

    fun unshift(value: T) {
        push(value)
        head = head?.next
    }

    fun pop(): T? {
        val retValue = head?.value

        if (head == head?.next) {
            head = null
        } else {
            val origHead = head

            head = head?.next
            head?.prev = origHead?.prev
            origHead?.prev?.next = head
        }

        return retValue
    }


    fun shift(): T? {
        head = head?.prev
        return pop()
    }

    override fun toString(): String =
        if (head != null) {
            val sb = StringBuilder()

            var cursor = head
            sb.append("${cursor?.value}")
            cursor = cursor?.next

            while (cursor != head) {
                sb.append(":${cursor?.value}")
                cursor = cursor?.next
            }

            "[$sb]"
        } else {
            "[]"
        }

}
