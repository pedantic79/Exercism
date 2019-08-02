use num_bigint::BigInt;
use num_integer::Integer;
use num_traits::{pow, Zero};

use std::cmp::Ordering;
use std::ops::{Add, Mul, Sub};
use std::str::FromStr;

/// Type implementing arbitrary-precision decimal arithmetic
#[derive(Clone, Debug)]
pub struct Decimal {
    number: BigInt,
    offset: usize,
}

fn normalize_offsets(l: &Decimal, r: &Decimal) -> (BigInt, BigInt, usize) {
    let ten = BigInt::from(10);

    if l.offset > r.offset {
        (
            l.number.clone(),
            r.number.clone() * pow(ten, l.offset - r.offset),
            l.offset,
        )
    } else {
        (
            l.number.clone() * pow(ten, r.offset - l.offset),
            r.number.clone(),
            r.offset,
        )
    }
}

impl Decimal {
    fn new(number: BigInt, offset: usize) -> Self {
        let ten = BigInt::from(10);

        if number == BigInt::zero() {
            Self { number, offset: 0 }
        } else {
            let mut number = number;
            let mut offset = offset;

            loop {
                let (num, rem) = number.div_rem(&ten);
                if offset > 0 && rem == BigInt::zero() {
                    number = num;
                    offset -= 1;
                } else {
                    break;
                }
            }

            Self { number, offset }
        }
    }

    pub fn try_from(input: &str) -> Option<Self> {
        if let Ok(number) = BigInt::from_str(
            input
                .chars()
                .filter(|c| *c != '.')
                .collect::<String>()
                .as_str(),
        ) {
            let offset = if let Some(pos) = input.find('.') {
                input.len() - pos - 1
            } else {
                0
            };

            Some(Self::new(number, offset))
        } else {
            None
        }
    }
}

impl Add for Decimal {
    type Output = Self;

    fn add(self, other: Decimal) -> Self {
        let (a, b, e) = normalize_offsets(&self, &other);
        Self::new(a + b, e)
    }
}

impl Sub for Decimal {
    type Output = Self;

    fn sub(self, other: Decimal) -> Self {
        let (a, b, e) = normalize_offsets(&self, &other);
        Self::new(a - b, e)
    }
}

impl Mul for Decimal {
    type Output = Self;

    #[allow(clippy::suspicious_arithmetic_impl)]
    fn mul(self, other: Decimal) -> Self {
        Self::new(self.number * other.number, self.offset + other.offset)
    }
}

impl PartialEq for Decimal {
    fn eq(&self, other: &Decimal) -> bool {
        self.number == other.number && self.offset == other.offset
    }
}

impl PartialOrd for Decimal {
    fn partial_cmp(&self, other: &Decimal) -> Option<Ordering> {
        let (a, b, _) = normalize_offsets(&self, &other);
        Some(a.cmp(&b))
    }
}
