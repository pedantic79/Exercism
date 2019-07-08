pub fn abbreviate(phrase: &str) -> String {
    phrase
        .split(|c: char| !c.is_ascii_alphabetic())
        .filter_map(|word| {
            let uppercase: String = word.chars().filter(|c| c.is_ascii_uppercase()).collect();

            match uppercase.len() {
                _ if uppercase.len() == word.len() => {
                    uppercase.chars().nth(0).map(|c| c.to_string())
                }
                0 => word
                    .chars()
                    .nth(0)
                    .map(|c| c.to_ascii_uppercase().to_string()),
                _ => Some(uppercase),
            }
        })
        .collect()
}
