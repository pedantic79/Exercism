use std::collections::HashMap;

const VALID: [char; 4] = ['A', 'C', 'G', 'T'];

pub fn count(nucleotide: char, dna: &str) -> Result<usize, char> {
    valid_nucleotide(nucleotide)?;

    let mut count = 0;

    for c in dna.chars() {
        valid_nucleotide(c)?;
        if c == nucleotide {
            count += 1;
        }
    }
    Ok(count)
}

pub fn nucleotide_counts(dna: &str) -> Result<HashMap<char, usize>, char> {
    let mut map: HashMap<char, usize> = VALID.iter().map(|&x| (x, 0)).collect();

    for c in dna.chars() {
        valid_nucleotide(c)?;
        *map.entry(c).or_insert(0) += 1;
    }
    Ok(map)
}

fn valid_nucleotide(nucleotide: char) -> Result<(), char> {
    if VALID.iter().any(|&c| c == nucleotide) {
        Ok(())
    } else {
        Err(nucleotide)
    }
}
