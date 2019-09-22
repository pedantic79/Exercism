use crate::Error;
use std::convert::TryFrom;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd)]
pub enum Suit {
    Clubs,
    Diamonds,
    Hearts,
    Spades,
}

impl TryFrom<&str> for Suit {
    type Error = Error;
    fn try_from(s: &str) -> Result<Self, Self::Error> {
        use Suit::*;
        match s {
            "S" => Ok(Spades),
            "H" => Ok(Hearts),
            "D" => Ok(Diamonds),
            "C" => Ok(Clubs),
            _ => Err(Error::InvalidSuit),
        }
    }
}
