mod rank;
mod suit;

use crate::Error;
use std::{cmp::Ordering, convert::TryFrom};

pub use rank::Rank;
pub use suit::Suit;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd)]
pub struct Card {
    pub rank: Rank,
    pub suit: Suit,
}

impl Card {
    pub fn new(r: Rank, s: Suit) -> Self {
        Self { rank: r, suit: s }
    }
}

impl TryFrom<&str> for Card {
    type Error = Error;

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        let l = s.len();

        if l == 2 || l == 3 {
            let split = (l + 1) / 2;
            let r = Rank::try_from(&s[..split])?;
            let t = Suit::try_from(&s[split..])?;

            Ok(Self::new(r, t))
        } else {
            Err(Error::InvalidCard)
        }
    }
}

impl Ord for Card {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap_or(Ordering::Less)
    }
}
