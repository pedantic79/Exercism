class Transcriptor {
    toRna(sequence: string): string {
        return sequence.toUpperCase().split('').map(
            (rna: string) => {
                switch (rna) {
                    case 'G': return 'C'
                    case 'C': return 'G'
                    case 'T': return 'A'
                    case 'A': return 'U'
                    default: throw new Error('Invalid input DNA.')
                }
            }
        ).join('')
    }
}

export default Transcriptor
