use num_integer::Integer;

/// While the problem description indicates a return status of 1 should be
/// returned on errors, it is much more common to return a `Result`, so we
/// provide an error type for the result here.
#[derive(Debug, Eq, PartialEq)]
pub enum AffineCipherError {
    NotCoprime(i32),
}

const ALPHABET_LENGTH: u8 = 26;

trait ToU8 {
    fn to_u8(self) -> u8;
}

/// Convert the `i32` to `u8` without losing data, by using `rem_euclid`.
/// This will will remap the number between `(0..ALPHABET_LENGTH)` ensuring it
/// will not overflow.
impl ToU8 for i32 {
    fn to_u8(self) -> u8 {
        self.rem_euclid(ALPHABET_LENGTH as Self) as u8
    }
}

/// Converts a `String` to a `Result`, I tried doing this with the `From` trait
/// but because `From` and `Result` are defined outside this crate, it won't
/// let me implement `impl From<String> for Result<String, AffineCipherError>`
trait ToOk<E: Sized> {
    type Output;

    fn ok(self) -> Result<Self::Output, E>;
}

impl<E> ToOk<E> for String {
    type Output = Self;

    fn ok(self) -> Result<Self::Output, E> {
        Ok(self)
    }
}

impl From<AffineCipherError> for Result<String, AffineCipherError> {
    fn from(error: AffineCipherError) -> Self {
        Err(error)
    }
}

/// Encodes the plaintext using the affine cipher with key (`a`, `b`). Note
/// that, rather than returning a return code, the more common convention in
/// Rust is to return a `Result`.
pub fn encode(plaintext: &str, a: i32, b: i32) -> Result<String, AffineCipherError> {
    if !is_coprime(a, ALPHABET_LENGTH.into()) {
        AffineCipherError::NotCoprime(a).into()
    } else {
        plaintext
            .chars()
            .filter(char::is_ascii_alphanumeric)
            .map(|c| {
                if c.is_ascii_alphabetic() {
                    let c = c.to_ascii_lowercase() as u8;
                    (encode_letter(c - b'a', a, b) + b'a').into()
                } else {
                    c
                }
            })
            .enumerate()
            .fold(String::new(), |mut s, (p, c)| {
                if p % 5 == 0 && p > 0 {
                    s.push(' ');
                }
                s.push(c);
                s
            })
            .ok()
    }
}

/// Decodes the ciphertext using the affine cipher with key (`a`, `b`). Note
/// that, rather than returning a return code, the more common convention in
/// Rust is to return a `Result`.
pub fn decode(ciphertext: &str, a: i32, b: i32) -> Result<String, AffineCipherError> {
    let a_mi = mult_inverse(a)?;
    let b_ai = add_inverse(b)?;

    ciphertext
        .chars()
        .filter(char::is_ascii_alphanumeric)
        .map(|c| {
            if c.is_ascii_alphabetic() {
                let c = i32::from((c as u8) - b'a');

                ((c * a_mi + b_ai * a_mi).to_u8() + b'a').into()
            } else {
                c
            }
        })
        .collect::<String>()
        .ok()
}

fn is_coprime(a: i32, b: i32) -> bool {
    a.gcd(&b) == 1
}

fn encode_letter(c: u8, a: i32, b: i32) -> u8 {
    (a * i32::from(c) + b).to_u8()
}

/// Calculate the modular multiplicative inverse
/// https://en.wikipedia.org/wiki/Modular_multiplicative_inverse
fn mult_inverse(a: i32) -> Result<i32, AffineCipherError> {
    let m = ALPHABET_LENGTH.into();

    (1..m)
        .find(|n| (a * n) % m == 1)
        .ok_or(AffineCipherError::NotCoprime(a))
}

/// Calculate the additive inverse
/// https://en.wikipedia.org/wiki/Additive_inverse
fn add_inverse(b: i32) -> Result<i32, AffineCipherError> {
    let m = ALPHABET_LENGTH.into();

    (1..m)
        .find(|n| (b + n) % m == 0)
        .ok_or(AffineCipherError::NotCoprime(b))
}
