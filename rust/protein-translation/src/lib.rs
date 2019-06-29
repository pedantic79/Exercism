use std::collections::HashMap;

pub struct CodonsInfo<'a> {
    codons: HashMap<&'a str, &'a str>,
}

impl<'a> CodonsInfo<'a> {
    pub fn name_for(&self, codon: &str) -> Option<&'a str> {
        self.codons.get(codon).cloned()
    }

    pub fn of_rna(&self, rna: &str) -> Option<Vec<&'a str>> {
        rna.chars()
            .collect::<Vec<_>>()
            .chunks(3)
            .map(|codon| {
                let c = codon.iter().collect::<String>();
                self.name_for(c.as_str())
            })
            .take_while(|x| {
                if let Some(s) = x {
                    *s != "stop codon"
                } else {
                    true
                }
            })
            .collect()
    }
}

pub fn parse<'a>(pairs: Vec<(&'a str, &'a str)>) -> CodonsInfo<'a> {
    let mut codons = HashMap::new();

    for (codon, name) in pairs.into_iter() {
        codons.insert(codon, name);
    }

    CodonsInfo { codons }
}
