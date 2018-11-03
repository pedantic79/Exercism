object BinarySearch {
    fun <T> search(list: List<T>, n: T): Int where T : Comparable<T> = searchPos(list, n, 0)
}

private fun <T> searchPos(list: List<T>, n: T, offset: Int): Int
        where T : Comparable<T> =
        when {
            list.isEmpty() -> -1
            list[0] == n   -> offset
            list.size == 1 -> -1 // This will be true if a size 1 list does not have our search value
            else -> {
                val midpoint = list.size / 2

                if (n < list[midpoint])
                    searchPos(list.subList(0, midpoint), n, offset)
                else
                    searchPos(list.subList(midpoint, list.size), n, offset + midpoint)
            }
        }
