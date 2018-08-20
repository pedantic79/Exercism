#[derive(Debug, PartialEq)]
pub enum Comparison {
    Equal,
    Sublist,
    Superlist,
    Unequal,
}

pub fn sublist<T: PartialEq>(a: &[T], b: &[T]) -> Comparison {
    if a == b {
        Comparison::Equal
    } else if contains(a, b) {
        Comparison::Superlist
    } else if contains(b, a) {
        Comparison::Sublist
    } else {
        Comparison::Unequal
    }
}

fn contains<T: PartialEq>(a: &[T], b: &[T]) -> bool {
    if b.len() == 0 {
        true
    } else if a.len() < b.len() {
        false
    } else {
        a.windows(b.len()).any(|s| s == b)
    }
}
