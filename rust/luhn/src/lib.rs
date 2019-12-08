/// Check a Luhn checksum.
pub fn is_valid(code: &str) -> bool {
    code.chars()
        .rev()
        .filter(|c| !c.is_whitespace())
        .try_fold((0, 0), |(s, idx), letter| {
            letter
                .to_digit(10)
                .map(|x| (s + double_if_second(x, idx), idx + 1))
        })
        .map_or(false, |(s, length)| length > 1 && s % 10 == 0)
}

fn double_if_second(num: u32, idx: u32) -> u32 {
    if idx % 2 == 1 {
        num * 2 - if num > 4 { 9 } else { 0 }
    } else {
        num
    }
}
