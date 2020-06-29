use std::collections::HashMap;

pub struct CodonsInfo<'a> {
    codons: HashMap<&'a str, &'a str>,
}

impl CodonsInfo<'_> {
    pub fn name_for(&self, codon: &str) -> Option<&str> {
        self.codons.get(codon).copied()
    }

    pub fn of_rna(&self, rna: &str) -> Option<Vec<&str>> {
        splitn(rna, 3)
            .map(|codon| self.name_for(codon))
            .take_while(|&x| Some("stop codon") != x)
            .collect()
    }
}

pub fn parse<'a>(pairs: Vec<(&'a str, &'a str)>) -> CodonsInfo<'a> {
    let codons = pairs.into_iter().collect();

    CodonsInfo { codons }
}

// Split Iterator, providing substrings of length `n`
fn splitn(s: &str, n: usize) -> impl Iterator<Item = &str> + '_ {
    let mut pos = 0;
    let size = s.len();

    std::iter::from_fn(move || {
        let start = pos;
        pos += n;

        if pos < size {
            Some(&s[start..pos])
        } else if start < size {
            Some(&s[start..])
        } else {
            None
        }
    })
}
