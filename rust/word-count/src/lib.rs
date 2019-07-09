use std::collections::HashMap;

/// Count occurrences of words.
pub fn word_count(words: &str) -> HashMap<String, u32> {
    words
        .split(|c: char| !c.is_alphanumeric() && c != '\'')
        .filter(|s| !s.is_empty())
        .fold(HashMap::new(), |mut hm, word| {
            let word = word.trim_matches('\'').to_lowercase();
            *hm.entry(word).or_insert(0) += 1;
            hm
        })
}
