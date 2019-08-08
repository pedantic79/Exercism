pub trait Luhn {
    fn valid_luhn(&self) -> bool;
}

impl<T> Luhn for T
where
    T: ToString,
{
    fn valid_luhn(&self) -> bool {
        self.to_string()
            .chars()
            .rev()
            .filter(|c| !c.is_whitespace())
            .try_fold((0, 0), |(s, idx), letter| {
                let dgt = letter.to_digit(10).map(|x| x * (idx % 2 + 1) % 9)?;

                Some((s + dgt, idx + 1))
            })
            .map_or(false, |(s, length)| length > 1 && s % 10 == 0)
    }
}
