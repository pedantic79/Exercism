/// Check a Luhn checksum.
pub fn is_valid(code: &str) -> bool {
    let sum =
        code.chars()
            .rev()
            .filter(|c| !c.is_whitespace())
            .try_fold((0, 0), |(s, idx), letter| {
                let dgt = letter.to_digit(10).map(|x| double_if_second(x, idx))?;

                Some((s + dgt, idx + 1))
            });

    sum.map_or(false, |(s, length)| length > 1 && s % 10 == 0)
}

fn double_if_second(num: u32, idx: u32) -> u32 {
    let n = if idx % 2 == 0 { num } else { num * 2 };

    if n > 9 {
        n - 9
    } else {
        n
    }
}
