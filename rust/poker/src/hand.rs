pub mod cardhand;
pub mod pokerhand;

use std::{
    cmp::Ordering,
    convert::{TryFrom, TryInto},
};

use crate::{Card, Error};
use cardhand::CardHand;
use pokerhand::PokerHand;

#[derive(Debug, PartialEq, Eq)]
pub struct Hand<'a> {
    pub source: &'a str,
    pub pokerhand: PokerHand,
}

impl<'a> Hand<'a> {
    pub fn try_new(src: &'a str, cards: &[Card]) -> Result<Self, Error> {
        let hand: CardHand = cards.try_into()?;

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
