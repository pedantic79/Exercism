use std::cmp::Ordering;

#[derive(Debug, PartialEq, Eq)]
pub enum Classification {
    Abundant,
    Perfect,
    Deficient,
}

pub fn classify(num: u64) -> Option<Classification> {
    if num < 1 {
        None
    } else {
        let s: u64 = (1..num).filter(|i| num % i == 0).sum();

        Some(match s.cmp(&num) {
            Ordering::Equal => Classification::Perfect,
            Ordering::Greater => Classification::Abundant,
            _ => Classification::Deficient,
        })
    }
}
