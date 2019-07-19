#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Nucleotide {
    Adenosine,
    Guanine,
    Cytosine,
    Thymidine,
    Uracil,
}

#[derive(Debug, PartialEq)]
pub struct DNA {
    sequence: Vec<Nucleotide>,
}

#[derive(Debug, PartialEq)]
pub struct RNA {
    sequence: Vec<Nucleotide>,
}

impl Nucleotide {
    fn try_new_dna(nucleotide: char) -> Option<Self> {
        match nucleotide {
            'A' => Some(Nucleotide::Adenosine),
            'G' => Some(Nucleotide::Guanine),
            'C' => Some(Nucleotide::Cytosine),
            'T' => Some(Nucleotide::Thymidine),
            _ => None,
        }
    }

    fn try_new_rna(nucleotide: char) -> Option<Self> {
        match nucleotide {
            'A' => Some(Nucleotide::Adenosine),
            'G' => Some(Nucleotide::Guanine),
            'C' => Some(Nucleotide::Cytosine),
            'U' => Some(Nucleotide::Uracil),
            _ => None,
        }
    }

    fn transpose(self) -> Self {
        match self {
            Nucleotide::Adenosine => Nucleotide::Uracil,
            Nucleotide::Cytosine => Nucleotide::Guanine,
            Nucleotide::Guanine => Nucleotide::Cytosine,
            Nucleotide::Thymidine => Nucleotide::Adenosine,
            Nucleotide::Uracil => unreachable!(),
        }
    }
}

impl DNA {
    pub fn new(dna: &str) -> Result<Self, usize> {
        dna.chars()
            .enumerate()
            .map(|(i, c)| Nucleotide::try_new_dna(c).ok_or_else(|| i))
            .collect::<Result<_, _>>()
            .map(|sequence| Self { sequence })
    }

    pub fn into_rna(self) -> RNA {
        let rna = self.sequence.into_iter().map(|r| r.transpose()).collect();
        RNA { sequence: rna }
    }
}

impl RNA {
    pub fn new(rna: &str) -> Result<Self, usize> {
        rna.chars()
            .enumerate()
            .map(|(i, c)| Nucleotide::try_new_rna(c).ok_or_else(|| i))
            .collect::<Result<_, _>>()
            .map(|sequence| Self { sequence })
    }
}
