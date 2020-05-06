pub mod fivecards;
pub mod pokerhand;

use std::{
    cmp::Ordering,
    convert::{TryFrom, TryInto},
};

use crate::{Card, Error};
use fivecards::FiveCards;
use pokerhand::PokerHand;

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct Hand<'a> {
    pub source: &'a str,
    pub pokerhand: PokerHand,
}

impl<'a> Hand<'a> {
    pub fn try_new(src: &'a str, cards: &[Card]) -> Result<Self, Error> {
        let hand: FiveCards = cards.try_into()?;

        Ok(Self {
            source: src,
            pokerhand: hand.into(),
        })
    }
}

impl<'a> TryFrom<&'a str> for Hand<'a> {
    type Error = Error;

    fn try_from(s: &'a str) -> Result<Self, Self::Error> {
        let cards = s
            .split_whitespace()
            .map(Card::try_from)
            .collect::<Result<Vec<Card>, Error>>()?;

        Self::try_new(s, &cards)
    }
}

impl<'a> Ord for Hand<'a> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.pokerhand.cmp(&other.pokerhand)
    }
}

impl<'a> PartialOrd for Hand<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.pokerhand.partial_cmp(&other.pokerhand)
    }
}

#[cfg(tests)]
mod tests {
    #[test]
    fn test_parse_hand() {
        let v = vec![
            Card::try_from("3S"),
            Card::try_from("3D"),
            Card::try_from("3C"),
            Card::try_from("3H"),
            Card::try_from("4H"),
        ]
        .iter()
        .map(|x| x.as_ref().unwrap())
        .copied()
        .collect::<Vec<Card>>();

        let h = Hand::try_new("3S 3D 3C 3H 4H", &v);

        assert_eq!(Hand::try_from("3S 3D 3C 3H 4H"), h);
        assert_eq!(Hand::try_from("3S 3D 3C 3H"), Err(Error::InvalidCardCount));
        assert_eq!(
            Hand::try_from("3S 3D 3C 3H 4H 5H"),
            Err(Error::InvalidCardCount)
        );
        assert_eq!(Hand::try_from("3S 3D 3C 3H 5Z"), Err(Error::InvalidSuit));
    }
}
