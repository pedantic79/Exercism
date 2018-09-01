class Matrix(private val matrix: List<List<Int>>) {
    val saddlePoints by lazy {
        var points = HashSet<MatrixCoordinate>()

        for ((rowIndex, row) in matrix.withIndex()) {
            for ((colIndex, cell) in row.withIndex()) {
                if (cell == getMaxRow(rowIndex) && cell == getMinCol(colIndex)) {
                    points.add(MatrixCoordinate(rowIndex, colIndex))
                }
            }
        }
        points
    }

    private fun getMaxRow(row: Int) = matrix[row].max() ?: 0
    private fun getMinCol(col: Int) = matrix.map { row -> row[col] }.min() ?: 0
}