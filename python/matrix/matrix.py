class Matrix(object):
    def __init__(self, matrix_string):
        self.matrix = [[int(cell) for cell in row.split()]
                       for row in matrix_string.splitlines()]

    def row(self, index):
        return self.matrix[index - 1]

    def column(self, index):
        return [row[index - 1] for row in self.matrix]
