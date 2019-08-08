/// Check a Luhn checksum.
pub fn is_valid(code: &str) -> bool {
    code.chars()
        .rev()
        .filter(|c| !c.is_whitespace())
        .try_fold((0, 0), |(s, idx), letter| {
            let dgt = letter.to_digit(10).map(|x| double_if_second(x, idx))?;

            Some((s + dgt, idx + 1))
        })
        .map_or(false, |(s, length)| length > 1 && s % 10 == 0)
}

fn double_if_second(num: u32, idx: u32) -> u32 {
    let n = num * (idx % 2 + 1);

    if n > 9 {
        n - 9
    } else {
        n
    }
}
