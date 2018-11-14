package matrix

import (
	"errors"
	"strconv"
	"strings"
)

type Matrix [][]int

func (m *Matrix) Rows() [][]int {
	matrix := make([][]int, len(*m))

	for r, row := range *m {
		matrix[r] = append([]int{}, row...)
	}
	return matrix
}

func (m *Matrix) Cols() [][]int {
	matrix := *m

	col := make([][]int, len(matrix[0]))
	for c := range col {
		col[c] = make([]int, len(matrix))
		for r := range col[c] {
			col[c][r] = matrix[r][c]
		}
	}

	return col
}

func (m *Matrix) Set(r, c, value int) bool {
	matrix := *m
	if r < 0 || r >= len(matrix) || c < 0 || c >= len(matrix[0]) {
		return false
	}

	matrix[r][c] = value
	return true
}

func New(s string) (*Matrix, error) {
	lines := strings.Split(s, "\n")
	matrix := make(Matrix, len(lines))

	for i, l := range lines {
		nums := strings.Fields(l)
		if i > 0 && len(nums) != len(matrix[0]) {
			return nil, errors.New("rows have unequal length")
		}

		row := make([]int, len(nums))

		for j, n := range nums {
			var err error
			if row[j], err = strconv.Atoi(n); err != nil {
				return nil, errors.New("invalid int in element data")
			}
		}
		matrix[i] = row
	}
	return &matrix, nil
}
