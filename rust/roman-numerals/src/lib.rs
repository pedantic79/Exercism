use std::fmt::{Display, Formatter, Result};

const ROMAN_NUMERALS: [(u32, &str); 13] = [
    (1000, "M"),
    (900, "CM"),
    (500, "D"),
    (400, "CD"),
    (100, "C"),
    (90, "XC"),
    (50, "L"),
    (40, "XL"),
    (10, "X"),
    (9, "IX"),
    (5, "V"),
    (4, "IV"),
    (1, "I"),
];

pub struct Roman(String);

impl Display for Roman {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.0)
    }
}

impl From<u32> for Roman {
    fn from(num: u32) -> Self {
        let mut num = num;
        let mut output = String::new();

        for (divisor, symbol) in ROMAN_NUMERALS.iter() {
            while num / divisor > 0 {
                output.push_str(symbol);
                num -= divisor;
            }
        }
        Self(output)
    }
}
