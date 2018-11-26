use std::collections::HashMap;

pub fn check(candidate: &str) -> bool {
    build_freq(&filt(candidate)).values().all(|&x| x == 1)
}

fn filt(s: &str) -> String {
    s.to_lowercase()
        .chars()
        .filter(|c| c.is_alphabetic())
        .collect()
}

fn build_freq(s: &str) -> HashMap<char, usize> {
    s.chars().fold(HashMap::new(), |mut acc, c| {
        *acc.entry(c).or_insert(0) += 1;
        acc
    })
}
