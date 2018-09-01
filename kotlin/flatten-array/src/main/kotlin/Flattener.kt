object Flattener {

    fun flatten(unflattened: Collection<Any?>): List<Any> =
            unflattened.filterNotNull().fold(mutableListOf()) { acc, value ->
                if (value is Collection<*>) {
                    acc.addAll(flatten(value))
                } else {
                    acc.add(value)
                }
                acc
            }

}

