pub fn abbreviate(phrase: &str) -> String {
    phrase
        .split(|c: char| !c.is_ascii_alphabetic() && c != '\'')
        .filter_map(|word| {
            let uppercase: String = word.chars().filter(|c| c.is_ascii_uppercase()).collect();

            if uppercase.is_empty() {
                word.chars()
                    .next()
                    .map(|c| c.to_ascii_uppercase().to_string())
            } else if uppercase.len() == word.len() {
                uppercase.chars().next().map(|c| c.to_string())
            } else {
                Some(uppercase)
            }
        })
        .collect()
}
