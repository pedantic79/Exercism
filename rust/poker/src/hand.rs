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

    #[test]
    fn test_is_straight() {
        use PokerHand::*;
        assert_eq!(
            PokerHand::from(mk_hand(["3S", "4D", "5C", "6H", "7H"])),
            Straight(7)
        );
        assert_eq!(
            PokerHand::from(mk_hand(["AS", "2D", "3C", "4H", "5H"])),
            Straight(5)
        );
        assert_eq!(
            PokerHand::from(mk_hand(["KS", "AD", "2C", "3H", "4H"])),
            HighCard([14, 13, 4, 3, 2])
        );
        assert_eq!(
            PokerHand::from(mk_hand(["KS", "QD", "10C", "JH", "AH"])),
            Straight(14)
        )
    }

    #[test]
    fn test_is_flush() {
        use PokerHand::*;
        assert_eq!(
            PokerHand::from(mk_hand(["3S", "4D", "5C", "6H", "7H"])),
            Straight(7)
        );
        assert_eq!(
            PokerHand::from(mk_hand(["4H", "2H", "3H", "9H", "5H"])),
            Flush([9, 5, 4, 3, 2])
        );
        assert_eq!(
            PokerHand::from(mk_hand(["4H", "AH", "3H", "6H", "5H"])),
            Flush([14, 6, 5, 4, 3])
        );
        assert_eq!(
            PokerHand::from(mk_hand(["3H", "QH", "10C", "JH", "AH"])),
            HighCard([14, 12, 11, 10, 3])
        )
    }

    #[test]
    fn test_is_sflush() {
        use PokerHand::*;
        assert_eq!(
            PokerHand::from(mk_hand(["3S", "4D", "5C", "6H", "8D"])),
            HighCard([8, 6, 5, 4, 3])
        );
        assert_eq!(
            PokerHand::from(mk_hand(["4H", "2H", "3H", "9H", "5H"])),
            Flush([9, 5, 4, 3, 2])
        );
        assert_eq!(
            PokerHand::from(mk_hand(["4H", "2H", "3H", "6H", "5H"])),
            StraightFlush(6)
        );
        assert_eq!(
            PokerHand::from(mk_hand(["KH", "QH", "10C", "JH", "AH"])),
            Straight(14)
        )
    }

    #[test]
    fn test_the_rest() {
        use PokerHand::*;
        assert_eq!(
            PokerHand::from(mk_hand(["3S", "3D", "3C", "4H", "4H"])),
            FullHouse(3, 4)
        );
        assert_eq!(
            PokerHand::from(mk_hand(["3S", "3D", "4C", "4H", "4H"])),
            FullHouse(4, 3)
        );
        assert_eq!(
            PokerHand::from(mk_hand(["3S", "3D", "AC", "4H", "4H"])),
            TwoPair([4, 3], 14)
        );
        assert_eq!(
            PokerHand::from(mk_hand(["3S", "3D", "AC", "KH", "10H"])),
            Pair(3, [14, 13, 10])
        );
    }

    #[test]
    fn test_simple_ord() {
        use PokerHand::*;
        assert!(FourKind(4, 2) < FourKind(14, 2));
        assert!(FourKind(4, 5) < StraightFlush(14));
        assert!(ThreeKind(14, [3, 2]) < FourKind(4, 14));
    }

    fn mk_card(card: &str) -> Card {
        Card::try_from(card).unwrap()
    }

    fn mk_hand(cards: [&str; 5]) -> FiveCards {
        let mut v = cards.iter().map(|c| mk_card(c)).collect::<Vec<Card>>();
        v.sort();
        FiveCards::try_from(v.as_ref()).unwrap()
    }
}
