package Hamming

fun compute(dna1: String, dna2: String): Int {
    require(dna1.length == dna2.length) {
        "left and right strands must be of equal length."
    }


    var count = 0
    for ((index, nucleotide1) in dna1.withIndex()) {
        val nucleotide2 = dna2[index]
        if (nucleotide1 != nucleotide2) {
            count++
        }
    }
    return count
}
