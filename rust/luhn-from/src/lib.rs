pub struct Luhn(String);

impl Luhn {
    pub fn is_valid(&self) -> bool {
        self.0
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

impl<T> From<T> for Luhn
where
    T: ToString,
{
    fn from(input: T) -> Self {
        Luhn(input.to_string())
    }
}
