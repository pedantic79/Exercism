use crate::Error;
use std::{cmp::Ordering, convert::TryFrom};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Rank {
    Num(u8),
    Jack,
    Queen,
    King,
    Ace,
}

impl Rank {
    pub fn value(self) -> u8 {
        use Rank::*;
        match self {
            Ace => 14,
            King => 13,
            Queen => 12,
            Jack => 11,
            Num(n) => n,
        }
    }
}

impl TryFrom<&str> for Rank {
    type Error = Error;
    fn try_from(s: &str) -> Result<Self, Self::Error> {
        use Rank::*;
        match s {
            "A" => Ok(Ace),
            "K" => Ok(King),
            "Q" => Ok(Queen),
            "J" => Ok(Jack),
            "10" => Ok(Num(10)),
            "9" => Ok(Num(9)),
            "8" => Ok(Num(8)),
            "7" => Ok(Num(7)),
            "6" => Ok(Num(6)),
            "5" => Ok(Num(5)),
            "4" => Ok(Num(4)),
            "3" => Ok(Num(3)),
            "2" => Ok(Num(2)),
            _ => Err(Error::InvalidRank),
        }
    }
}

impl PartialOrd for Rank {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.value().cmp(&other.value()))
    }
}
