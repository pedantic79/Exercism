#[derive(Debug, PartialEq)]
pub enum Error {
    NotEnoughPinsLeft,
    GameComplete,
}

#[derive(Default)]
pub struct BowlingGame {
    frames: Vec<u16>,
    roll_again: bool,
}

impl BowlingGame {
    pub fn new() -> Self {
        Self {
            frames: Vec::with_capacity(21),
            roll_again: false,
        }
    }

    pub fn roll(&mut self, pins: u16) -> Result<(), Error> {
        if pins > 10 || self.roll_again && pins + self.frames.last().unwrap() > 10 {
            return Err(Error::NotEnoughPinsLeft);
        } else if self.score().is_some() {
            return Err(Error::GameComplete);
        }

        self.frames.push(pins);
        self.roll_again = pins != 10 && !self.roll_again;

        Ok(())
    }

    pub fn score(&self) -> Option<u16> {
        let mut total = 0;
        let mut idx = 0;

        for _ in 0..10 {
            let a = self.frames.get(idx).copied()?;
            let b = self.frames.get(idx + 1).copied()?;

            total += a + b;
            if a == 10 || a + b == 10 {
                total += self.frames.get(idx + 2).copied()?;
            }
            idx += if a == 10 { 1 } else { 2 };
        }

        Some(total)
    }
}
