/// Check a Luhn checksum.
pub fn is_valid(code: &str) -> bool {
    let sum = code
                .chars()
                .rev()
                .fold(Some((0, 0)), |acc, letter| {
                    if char::is_whitespace(letter) {
                        return acc
                    }

                    if ! char::is_numeric(letter) {
                        return None
                    }

                    acc.map(|(s, idx)| {
                        let dgt = char::to_digit(letter, 10)
                                        .map(|x| double_if_second(x, idx))
                                        .unwrap();
                        (s + dgt, idx + 1)
                    })
                });

    match sum {
        Some((s, l)) => {
            if l < 2 {
                false
            } else {
                s % 10 == 0
            }
        },
        None => false,
    }
}

fn double_if_second(num: u32, idx: u32) -> u32 {
    if idx % 2 == 0 {
        num
    } else {
        let doubled = num * 2;
        if doubled > 9 {
            doubled - 9
        } else {
            doubled
        }
    }
}
