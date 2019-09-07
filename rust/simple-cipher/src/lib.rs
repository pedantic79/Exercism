use rand::Rng;

pub fn encode(key: &str, s: &str) -> Option<String> {
    encode_decode(|a, b| a + b, key, s)
}

pub fn decode(key: &str, s: &str) -> Option<String> {
    encode_decode(|a, b| a - b, key, s)
}

fn encode_decode<F>(op: F, key: &str, s: &str) -> Option<String>
where
    F: Fn(u8, u8) -> u8,
{
    s.chars()
        .zip(key.chars().cycle())
        .map(|pair| codec(&op, pair))
        .collect::<Option<String>>()
        .filter(|x| !x.is_empty())
}

pub fn encode_random(s: &str) -> (String, String) {
    let mut rng = rand::thread_rng();
    let key: String = (0..=100)
        .map(|_| (rng.gen_range::<u8>(0, 26) + b'a') as char)
        .collect();
    let encrypted = encode(&key, s);
    (key, encrypted.unwrap())
}

fn codec<F>(op: &F, (letter, key): (char, char)) -> Option<char>
where
    F: Fn(u8, u8) -> u8,
{
    if letter.is_ascii_lowercase() && key.is_ascii_lowercase() {
        let key = key as u8 - b'a';
        let letter = letter as u8 - b'a' + 26;
        Some((op(letter, key) % 26 + b'a') as char)
    } else {
        None
    }
}
