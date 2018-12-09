/// "Encipher" with the Atbash cipher.
pub fn encode(plain: &str) -> String {
        plain.chars()
                .filter(|x| x.is_ascii())
                .filter(|x| x.is_alphanumeric())
                .map(|x| x.to_ascii_lowercase())
                .map(at_bash)
                .collect::<Vec<char>>()
                .chunks(5)
                .map(|group| group.iter().collect::<String>())
                .collect::<Vec<String>>()
                .join(" ")
}

/// "Decipher" with the Atbash cipher.
pub fn decode(cipher: &str) -> String {
        cipher.chars()
                .filter(|x| x.is_alphanumeric())
                .map(at_bash)
                .collect::<String>()
}

pub fn at_bash(ch: char) -> char {
        if ch.is_numeric() {
                ch
        } else {
                let a = b'a';
                let z = b'z';
                (a + z - ch as u8) as char
        }
}
