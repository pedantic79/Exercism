mod rank;
mod suit;

use crate::Error;
use std::{cmp::Ordering, convert::TryFrom};

pub(crate) use rank::Rank;
pub(crate) use suit::Suit;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd)]
pub(crate) struct Card {
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
            Err(Error::Card)
        }
    }
}

impl Ord for Card {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap_or(Ordering::Less)
    }
}

#[cfg(tests)]
mod tests {
    fn suit_map<'a>() -> HashMap<&'a str, Suit> {
        use Suit::*;

        hashmap!(
            "D" => Diamonds,
            "C" => Clubs,
            "H" => Hearts,
            "S" => Spades,
        )
    }

    #[test]
    fn test_parse_card() {
        rank_map().iter().for_each(|(r, rank_exp)| {
            suit_map().iter().for_each(|(s, suit_exp)| {
                let card = format!("{}{}", r, s);
                let exp = poker::card::Card::new(*rank_exp, *suit_exp);
                assert_eq!(Card::try_from(&card[..]), Ok(exp));
            })
        });

        assert_eq!(Card::try_from("1S"), Err(Error::InvalidRank));
        assert_eq!(Card::try_from("AE"), Err(Error::InvalidSuit));
    }
}
