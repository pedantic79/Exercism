class Matrix(object):
    def __init__(self, matrix_string):
        self.matrix = []
        for row in matrix_string.split('\n'):
            r = [int(cell) for cell in row.split(' ')]
            self.matrix.append(r)

    def row(self, index):
        return self.matrix[index - 1]

    def column(self, index):
        return [row[index - 1] for row in self.matrix]
