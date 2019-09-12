#[derive(Debug, PartialEq)]
pub enum Error {
    IncompleteNumber,
    Overflow,
}

/// Convert a list of numbers to a stream of bytes encoded with variable length encoding.
pub fn to_bytes(values: &[u32]) -> Vec<u8> {
    values.iter().fold(Vec::new(), |mut v, x| {
        v.extend(encode(*x));
        v
    })
}

/// Given a stream of bytes, extract all numbers which are encoded in there.
pub fn from_bytes(bytes: &[u8]) -> Result<Vec<u32>, Error> {
    let len = bytes.len();

    bytes
        .iter()
        .enumerate()
        .try_fold((Vec::new(), 0u32), |(mut acc, n), (idx, b)| {
            let mut m = n << 7;

            if n == m >> 7 {
                m |= u32::from(b & 0x7f);

                if b & 0x80 == 0 {
                    acc.push(m);
                    Ok((acc, 0))
                } else if idx + 1 == len {
                    Err(Error::IncompleteNumber)
                } else {
                    Ok((acc, m))
                }
            } else {
                Err(Error::Overflow)
            }
        })
        .map(|(x, _)| x)
}

fn encode(n: u32) -> Vec<u8> {
    let mut v = vec![(n & 0x7f) as u8];
    let mut n = n >> 7;

    while n > 0 {
        v.push((n & 0x7f | 0x80) as u8);
        n >>= 7;
    }
    v.reverse();
    v
}
