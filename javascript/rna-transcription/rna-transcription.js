const rnaMapping = {
  'G': 'C',
  'C': 'G',
  'T': 'A',
  'A': 'U'
};

export const toRna = (sequence) =>
  sequence.split('').map(letter => rnaMapping[letter]).join('');

