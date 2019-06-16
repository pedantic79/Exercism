export const translate = (rna) => {
  let proteinSequence = [];

  if (rna) {
    const codonSequence = rna.match(/[GUAC]{1,3}/g);

    if (!codonSequence) {
      throw Error('Invalid codon');
    }

    for (const codon of codonSequence) {
      const protein = PROTEINS[codon];
      if (protein === 'STOP') {
        break
      } else {
        proteinSequence.push(protein);
      }
    }
  }

  return proteinSequence;
};

const PROTEINS = {
  AUG: 'Methionine',
  UUU: 'Phenylalanine',
  UUC: 'Phenylalanine',
  UUA: 'Leucine',
  UUG: 'Leucine',
  UCU: 'Serine',
  UCC: 'Serine',
  UCA: 'Serine',
  UCG: 'Serine',
  UAU: 'Tyrosine',
  UAC: 'Tyrosine',
  UGU: 'Cysteine',
  UGC: 'Cysteine',
  UGG: 'Tryptophan',
  UAA: 'STOP',
  UAG: 'STOP',
  UGA: 'STOP',
};
