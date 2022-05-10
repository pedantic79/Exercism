/// `Palindrome` is a newtype which only exists when the contained value is a palindrome number in base ten.
///
/// A struct with a single field which is used to constrain behavior like this is called a "newtype", and its use is
/// often referred to as the "newtype pattern". This is a fairly common pattern in Rust.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Palindrome(u64);

impl Palindrome {
    /// Create a `Palindrome` only if `value` is in fact a palindrome when represented in base ten. Otherwise, `None`.
    pub fn new(value: u64) -> Option<Palindrome> {
        if is_palindrome(value) {
            Some(Palindrome(value))
        } else {
            None
        }
    }

    /// Get the value of this palindrome.
    pub fn into_inner(self) -> u64 {
        self.0
    }
}

pub fn palindrome_products(min: u64, max: u64) -> Option<(Palindrome, Palindrome)> {
    let mut mn = None;
    let mut mx = None;

    for i in min..=max {
        for j in i..=max {
            let mult = Some(i * j);

            if mult < mn || mult > mx && is_palindrome(mult.unwrap()) {
                mn = mn.min(mult).or(mult);
                mx = mx.max(mult).or(mult);
            }
        }
    }

    mn.map(Palindrome).zip(mx.map(Palindrome))
}

fn is_palindrome(num: u64) -> bool {
    let s = num.to_string();
    let bytes = s.as_bytes();
    let l = bytes.len();
    for idx in 0..l / 2 {
        if bytes[idx] != bytes[l - 1 - idx] {
            return false;
        }
    }
    true
}
