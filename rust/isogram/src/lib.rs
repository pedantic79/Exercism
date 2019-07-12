use std::collections::HashSet;

pub fn check(candidate: &str) -> bool {
    let mut hs = HashSet::new();

    candidate
        .chars()
        .map(|c| c.to_ascii_lowercase())
        .filter(|c| c.is_alphabetic())
        .all(|c| hs.insert(c))
}
