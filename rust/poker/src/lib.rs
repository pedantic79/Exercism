pub mod card;
pub mod hand;

use card::Card;
use hand::Hand;
use std::convert::TryFrom;

#[derive(Debug, PartialEq)]
pub(crate) enum Error {
    Card,
    CardCount,
    Rank,
    Suit,
}

pub fn winning_hands<'a>(hands: &[&'a str]) -> Option<Vec<&'a str>> {
    let mut h = hands
        .iter()
        .map(|h| Hand::try_from(*h))
        .collect::<Result<Vec<Hand>, Error>>()
        .ok()?;

    h.sort_by(|a, b| b.cmp(&a));
    let winner = h.first()?;
    let f = h
        .iter()
        .take_while(|x| x.pokerhand == winner.pokerhand)
        .map(|x| x.source)
        .collect();

    Some(f)
}
