use std::collections::HashMap;
use std::collections::HashSet;

#[inline]
pub fn anagrams_for<'a>(word: &str, possible_anagrams: &[&'a str]) -> HashSet<&'a str> {
    // Vec is fastest
    anagrams_vec(word, possible_anagrams)
}

#[inline]
pub fn anagrams_sort<'a>(word: &str, possible_anagrams: &[&'a str]) -> HashSet<&'a str> {
    anagrams_fn(word, possible_anagrams, |s| {
        let mut v = s.chars().collect::<Vec<char>>();
        v.sort();
        v.iter().collect::<String>()
    })
}

#[inline]
pub fn anagrams_map<'a>(word: &str, possible_anagrams: &[&'a str]) -> HashSet<&'a str> {
    anagrams_fn(word, possible_anagrams, |s| {
        let mut m = HashMap::new();
        for c in s.chars() {
            *m.entry(c).or_insert(1) += 1;
        }
        m
    })
}

#[inline]
pub fn anagrams_vec<'a>(word: &str, possible_anagrams: &[&'a str]) -> HashSet<&'a str> {
    anagrams_fn(word, possible_anagrams, |s| {
        let mut v = s.chars().collect::<Vec<char>>();
        v.sort();
        v
    })
}

pub fn anagrams_fn<'a, T, F>(
    word: &str,
    possible_anagrams: &[&'a str],
    mut f: F,
) -> HashSet<&'a str>
where
    T: Eq,
    F: FnMut(&str) -> T,
{
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
