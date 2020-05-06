use crate::Error;
use std::convert::TryFrom;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd)]
pub(crate) enum Suit {
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
            _ => Err(Error::Suit),
        }
    }
}

#[cfg(tests)]
mod tests {
    #[test]
    fn test_parse_suit() {
        suit_map()
            .iter()
            .for_each(|(&k, v)| assert_eq!(Suit::try_from(k).as_ref(), Ok(v)));

        assert_eq!(Suit::try_from("A"), Err(Error::InvalidSuit));
    }
}
