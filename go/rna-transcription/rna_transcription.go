package strand

import (
	"strings"
)

//ToRNA converts DNA string to RNA
func ToRNA(dna string) string {
	var sb strings.Builder
	for _, rna := range dna {
		sb.WriteRune(getRNAFromDNA(rna))
	}
	return sb.String()
}

func getRNAFromDNA(dna rune) (rna rune) {
	switch dna {
	case 'C':
		rna = 'G'
	case 'G':
		rna = 'C'
	case 'T':
		rna = 'A'
	case 'A':
		rna = 'U'
	}
	return
}
