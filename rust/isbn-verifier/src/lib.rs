/// Determines whether the supplied string is a valid ISBN number
pub fn is_valid_isbn(isbn: &str) -> bool {
    isbn_checksum(isbn).is_some_and(|n| n.is_multiple_of(11))
}

fn isbn_checksum(isbn: &str) -> Option<u32> {
    let v = isbn
        .chars()
        .filter(|&c| c != '-')
        .map(char_to_digit)
        .collect::<Option<Vec<_>>>()?;

    if v.len() != 10 {
        None
    } else {
        v.into_iter()
            .zip((1..11).rev())
            .map(mult)
            .try_fold(0, |acc, x_opt| x_opt.map(|x| acc + x))
    }
}

fn char_to_digit(ch: char) -> Option<u32> {
    match ch {
        'X' => Some(10),
        _ if ch.is_ascii_digit() => ch.to_digit(10),
        _ => None,
    }
}

fn mult((digit, pos): (u32, u32)) -> Option<u32> {
    if digit == 10 && pos != 1 {
        None
    } else {
        Some(digit * pos)
    }
}
