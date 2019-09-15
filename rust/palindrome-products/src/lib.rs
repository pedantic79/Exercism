pub mod products;

use products::palindrome_products_mutex;
use std::collections::BTreeSet;

#[derive(Debug, PartialEq, Eq, Clone)]
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

#[inline(always)]
pub fn palindrome_products(min: u64, max: u64) -> Option<(Palindrome, Palindrome)> {
    palindrome_products_mutex(min, max)
}
