pub fn rotate(input: &str, key: i8) -> String {
    input.chars().map(|c| rot(key, c)).collect()
}

fn rot(key: i8, c: char) -> char {
    if c.is_ascii_alphabetic() {
        let case = if c.is_ascii_lowercase() { b'a' } else { b'A' };
        let key = (if key < 0 { 26 + key } else { key }) as u8;

        ((c as u8 - case + key) % 26 + case) as char
    } else {
        c
    }
}
