package dna

import (
	"errors"
)

// Histogram is a mapping from nucleotide to its count in given DNA.
type Histogram map[rune]int

// DNA is a list of nucleotides.
type DNA string

// Counts generates a histogram of valid nucleotides in the given DNA.
// Returns an error if d contains an invalid nucleotide.
func (d DNA) Counts() (Histogram, error) {
	h := Histogram{'A': 0, 'C': 0, 'G': 0, 'T': 0}

	for _, nucleotide := range d {
		if !verifyDNA(nucleotide) {
			return Histogram{}, errors.New("Invalid nucleotide")
		}
		h[nucleotide]++
	}
	return h, nil
}

func verifyDNA(nt rune) bool {
	switch nt {
	case 'A', 'G', 'C', 'T':
		return true
	}
	return false
}
