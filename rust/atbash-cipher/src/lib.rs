use std::iter::{from_fn, once};

/// "Encipher" with the Atbash cipher.
pub fn encode(plain: &str) -> String {
    split_from_fn(at_bash(plain))
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

/// split_flat_map is public so I can run benchmarks
/// flat_map   12:       224ns (R²=1.000, 4685942 iterations in 136 samples)
/// flat_map   48:       625ns (R²=0.994, 1806626 iterations in 126 samples)
/// flat_map  252:   1us 888ns (R²=0.995, 575638 iterations in 114 samples)
/// flat_map 1024:   5us 739ns (R²=0.999, 183408 iterations in 102 samples)
pub fn split_flat_map(itr: impl Iterator<Item = char>) -> String {
    itr.enumerate()
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

/// split_from_fn is public so I can run benchmarks
/// from_fn    12:       188ns (R²=1.000, 5669992 iterations in 138 samples)
/// from_fn    48:       441ns (R²=0.996, 2404623 iterations in 129 samples)
/// from_fn   252:       965ns (R²=1.000, 1121768 iterations in 121 samples)
/// from_fn  1024:   2us 444ns (R²=1.000, 432483 iterations in 111 samples)
pub fn split_from_fn(iter: impl Iterator<Item = char>) -> String {
    let mut count = 0;
    let mut iter = iter.peekable();

    from_fn(|| {
        if count == 5 && iter.peek().is_some() {
            count = 0;
            Some(' ')
        } else {
            count += 1;
            iter.next()
        }
    })
    .collect()
}
