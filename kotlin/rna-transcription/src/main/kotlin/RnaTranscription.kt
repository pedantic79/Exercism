fun transcribeToRna(dna: String): String =
    dna.map {x -> translate(x)}.joinToString("")

fun translate(nucleotide: Char): Char =
    when (nucleotide) {
        'G'  -> 'C'
        'C'  -> 'G'
        'T'  -> 'A'
        else -> 'U'
    }

