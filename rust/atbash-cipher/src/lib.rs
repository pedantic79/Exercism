/// "Encipher" with the Atbash cipher.
pub fn encode(plain: &str) -> String {
    plain
        .chars()
        .filter(|x| x.is_ascii())
        .filter(|x| x.is_alphanumeric())
        .map(|x| x.to_ascii_lowercase())
        .map(|x| at_bash(x))
        .collect::<Vec<char>>()
        .chunks(5)
        .map(|group| group.iter().collect::<String>())
        .collect::<Vec<String>>()
        .join(" ")
}

/// "Decipher" with the Atbash cipher.
pub fn decode(cipher: &str) -> String {
    cipher
        .chars()
        .filter(|x| x.is_alphanumeric())
        .map(|x| at_bash(x))
        .collect::<String>()
}

pub fn at_bash(ch: char) -> char {
    if ch.is_numeric() {
        ch
    } else {
        let a = 'a' as u8;
        let z = 'z' as u8;
        (a + z - ch as u8) as char
    }
}
