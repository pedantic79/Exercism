export const toRna = (sequence) => {
  return sequence.toUpperCase().split('').map(
    (rna) => {
      switch (rna) {
        case 'G': return 'C';
        case 'C': return 'G';
        case 'T': return 'A';
        case 'A': return 'U';
        default: throw new Error('Invalid input DNA.');
      }
    }
  ).join('');
}
