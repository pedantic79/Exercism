use std::collections::{hash_map::Entry, HashMap};

const VALID: [char; 4] = ['A', 'C', 'G', 'T'];

pub fn count(nucleotide: char, dna: &str) -> Result<usize, char> {
    valid_nucleotide(nucleotide)?;
    let hm = nucleotide_counts(dna)?;
    Ok(*hm.get(&nucleotide).unwrap_or(&0))
}

pub fn nucleotide_counts(dna: &str) -> Result<HashMap<char, usize>, char> {
    let mut map: HashMap<char, usize> = VALID.iter().map(|&x| (x, 0)).collect();

    for c in dna.chars() {
        match map.entry(c) {
            Entry::Occupied(mut x) => *x.get_mut() += 1,
            Entry::Vacant(_) => return Err(c),
        }
    }
    Ok(map)
}

fn valid_nucleotide(nucleotide: char) -> Result<(), char> {
    if VALID.contains(&nucleotide) {
        Ok(())
    } else {
        Err(nucleotide)
    }
}
