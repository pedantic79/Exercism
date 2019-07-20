use std::cmp::{Ord, Ordering};

#[derive(Debug, PartialEq)]
pub enum Comparison {
    Equal,
    Sublist,
    Superlist,
    Unequal,
}

pub fn sublist<T: PartialEq>(a: &[T], b: &[T]) -> Comparison {
    match a.len().cmp(&b.len()) {
        Ordering::Equal if a == b => Comparison::Equal,
        Ordering::Less if contains(b, a) => Comparison::Sublist,
        Ordering::Greater if contains(a, b) => Comparison::Superlist,
        _ => Comparison::Unequal,
    }
}

fn contains<T: PartialEq>(a: &[T], b: &[T]) -> bool {
    b.is_empty() || a.windows(b.len()).any(|s| s == b)
}
