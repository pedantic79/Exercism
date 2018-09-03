use std::collections::HashMap;
use std::collections::HashSet;

fn sort_string(s: &str) -> String {
    let mut v = s.chars().collect::<Vec<char>>();
    v.sort();
    v.iter().collect()
}

fn get_frequency(s: &str) -> HashMap<char, usize> {
    let mut m = HashMap::new();
    for c in s.chars() {
        *m.entry(c).or_insert(1) += 1;
    }
    m
}

#[inline]
pub fn anagrams_for<'a>(word: &str, possible_anagrams: &[&'a str]) -> HashSet<&'a str> {
    // Sort is faster
    anagrams_sort(word, possible_anagrams)
}

#[inline]
pub fn anagrams_sort<'a>(word: &str, possible_anagrams: &[&'a str]) -> HashSet<&'a str> {
    anagrams_fn(word, possible_anagrams, sort_string)
}

#[inline]
pub fn anagrams_map<'a>(word: &str, possible_anagrams: &[&'a str]) -> HashSet<&'a str> {
    anagrams_fn(word, possible_anagrams, get_frequency)
}

pub fn anagrams_fn<'a, T: Eq>(
    word: &str,
    possible_anagrams: &[&'a str],
    f: fn(&str) -> T,
) -> HashSet<&'a str> {
    let lower = word.to_lowercase();
    let fnstr = f(&lower);

    possible_anagrams
        .iter()
        .filter(|anagram| {
            let l = anagram.to_lowercase();
            lower != l && fnstr == f(&l)
        })
        .cloned()
        .collect()
}
