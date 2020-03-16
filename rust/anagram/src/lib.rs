use std::collections::{BTreeMap, HashSet};

pub use anagrams_vec as anagrams_for;

pub fn anagrams_map<'a>(word: &str, possible_anagrams: &[&'a str]) -> HashSet<&'a str> {
    anagrams_fn(word, possible_anagrams, |s| {
        s.chars().fold(BTreeMap::new(), |mut hm, c| {
            *hm.entry(c).or_insert(0) += 1;
            hm
        })
    })
}

pub fn anagrams_vec<'a>(word: &str, possible_anagrams: &[&'a str]) -> HashSet<&'a str> {
    anagrams_fn(word, possible_anagrams, |s| {
        let mut v = s.chars().collect::<Vec<char>>();
        v.sort();
        v
    })
}

#[inline]
fn anagrams_fn<'a, T, F>(word: &str, possible_anagrams: &[&'a str], mut f: F) -> HashSet<&'a str>
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

            // slight performance increase by adding the length comparision
            // since we can avoid doing that check if the lengths are not equal
            lower != l && lower.len() == l.len() && fnstr == f(&l)
        })
        .copied()
        .collect()
}
