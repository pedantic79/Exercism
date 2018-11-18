object BinarySearch {
    tailrec fun <T> search(list: List<T>, n: T, start: Int = 0, end: Int = list.size): Int where T : Comparable<T> =
            (end - start).let { size ->
                if (size == 0) -1
                else {
                    val midpoint = start + size / 2
                    when {
                        n == list[midpoint] -> midpoint
                        n < list[midpoint] -> search(list, n, start, midpoint)
                        else -> search(list, n, midpoint + 1, end)
                    }
                }
            }
}