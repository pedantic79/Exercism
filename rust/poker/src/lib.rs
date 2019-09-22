pub mod card;
pub mod hand;

use card::Card;
use hand::cardhand::CardHand;
use hand::Hand;
use std::convert::TryFrom;

#[derive(Debug, PartialEq)]
pub enum Error {
    InvalidCard,
    InvalidCardCount,
    InvalidRank,
    InvalidSuit,
}

pub fn winning_hands<'a>(hands: &[&'a str]) -> Option<Vec<&'a str>> {
    let mut h = hands
        .iter()
        .map(|h| Hand::try_from(*h))
        .collect::<Result<Vec<Hand>, Error>>()
        .ok()?;
    h.sort();

    let winner = h.last()?;
    let f = h
        .iter()
        .filter(|x| x.pokerhand == winner.pokerhand)
        .map(|x| x.source)
        .collect();

    Some(f)
}
