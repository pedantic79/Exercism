use rayon::iter::{IntoParallelIterator, ParallelIterator};
use std::collections::{BTreeMap, BTreeSet};

#[derive(Debug, PartialEq, Eq)]
pub struct Palindrome(BTreeSet<(u64, u64)>, u64);

impl Palindrome {
    pub fn new(a: u64, b: u64) -> Self {
        Self(std::iter::once((a, b)).collect(), a * b)
    }

    pub fn value(&self) -> u64 {
        self.1
    }

    pub fn insert(&mut self, a: u64, b: u64) {
        self.0.insert((a, b));
    }

    fn insert_palindrome(&mut self, other: &Self) {
        for (k, v) in other.0.iter() {
            self.insert(*k, *v)
        }
    }
}

pub fn palindrome_products(min: u64, max: u64) -> Option<(Palindrome, Palindrome)> {
    // let map: BTreeMap<u64, Palindrome> = (min..=max)
    //     .flat_map(|x| (x..=max).map(move |y| (x, y)))
    //     .filter(|(i, j)| is_palindrome(i * j))
    //     .fold(BTreeMap::new(), |mut acc, (a, b)| {
    //         acc.entry(a * b)
    //             .and_modify(|e| e.insert(a, b))
    //             .or_insert_with(|| Palindrome::new(a, b));
    //         acc
    //     });

    let map: BTreeMap<u64, Palindrome> = (min..=max)
        .into_par_iter()
        .flat_map(|i| (i..=max).into_par_iter().map(move |j| (i, j)))
        .filter(|(i, j)| is_palindrome(i * j))
        .map(new_palindrome_map)
        .reduce(BTreeMap::new, |mut acc, m| {
            for (k, v) in m {
                acc.entry(k)
                    .and_modify(|existing_v| existing_v.insert_palindrome(&v))
                    .or_insert(v);
            }
            acc
        });

    let mut p = map.into_iter().map(|(_, v)| v);
    Some((p.next()?, p.last()?))
}

fn is_palindrome(num: u64) -> bool {
    let s = num.to_string();
    let bytes = s.as_bytes();
    let l = bytes.len();

    for idx in 0..l / 2 {
        if bytes[idx] != bytes[l - 1 - idx] {
            return false;
        }
    }
    true
}

fn new_palindrome_map((a, b): (u64, u64)) -> BTreeMap<u64, Palindrome> {
    let mut bm = BTreeMap::new();
    bm.insert(a * b, Palindrome::new(a, b));
    bm
}

#[cfg(test)]
mod test {
    #[test]
    fn test_even_palindromes() {
        for i in &[22, 4334, 643_346, 82_011_028] {
            assert!(super::is_palindrome(*i));
        }
    }

    #[test]
    fn test_odd_palindromes() {
        for i in &[1, 4, 8, 121, 393, 52325, 7_203_027] {
            assert!(super::is_palindrome(*i));
        }
    }

    #[test]
    fn test_non_palindromes() {
        for i in &[12, 21, 122, 312, 5235, 5332] {
            assert!(!super::is_palindrome(*i));
        }
    }
}
