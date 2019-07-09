use std::iter::once;

/// "Encipher" with the Atbash cipher.
pub fn encode(plain: &str) -> String {
    at_bash(plain)
        .enumerate()
        .flat_map(|(idx, c)| {
            if idx % 5 == 0 && idx > 0 {
                Some(' ')
            } else {
                None
            }
            .into_iter()
            .chain(once(c))
        })
        .collect()
}

/// "Decipher" with the Atbash cipher.
pub fn decode(cipher: &str) -> String {
    at_bash(cipher).collect()
}

fn at_bash(s: &str) -> impl Iterator<Item = char> + '_ {
    s.chars()
        .filter(|c| c.is_ascii_alphanumeric())
        .map(|c| c.to_ascii_lowercase())
        .map(|c| {
            if c.is_numeric() {
                c
            } else {
                (b'a' + b'z' - c as u8) as char
            }
        })
}
