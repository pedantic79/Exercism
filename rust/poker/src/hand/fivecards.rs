use crate::{
    card::{Card, Rank},
    hand::pokerhand::PokerHand,
    Error,
};
use std::{collections::HashMap, convert::TryFrom};

/// FiveCards represents plain playing cards.
/// It has methods for evaluating the five cards, to determine the best PokerHand
pub(crate) struct FiveCards([Card; 5]);

impl TryFrom<&[Card]> for FiveCards {
    type Error = super::Error;

    fn try_from(cards: &[Card]) -> Result<Self, Self::Error> {
        if cards.len() != 5 {
            return Err(Error::CardCount);
        }

        let mut c = cards.to_owned();
        c.sort();
        Ok(Self([c[0], c[1], c[2], c[3], c[4]]))
    }
}

impl FiveCards {
    // Checks for 4-kind, fullhouse, 3-kind, etc.
    fn try_mult_cards(sorted: &[(u8, usize)]) -> Option<PokerHand> {
        use PokerHand::*;
        let sizes = sorted.iter().map(|x| x.1).collect::<Vec<_>>();
        let val = sorted.iter().map(|x| x.0).collect::<Vec<_>>();
        // could write this as a giant if-else-if...-else block
        match sorted.len() {
            2 => match sizes.as_slice() {
                [4, 1] => Some(FourKind(val[0], val[1])),
                [3, 2] => Some(FullHouse(val[0], val[1])),
                _ => None,
            },

            3 => match sizes.as_slice() {
                [3, 1, 1] => Some(ThreeKind(val[0], [val[2], val[1]])),
                [2, 2, 1] => Some(TwoPair([val[1], val[0]], val[2])),
                _ => None,
            },

            4 => match sizes.as_slice() {
                [2, 1, 1, 1] => Some(Pair(val[0], [val[3], val[2], val[1]])),
                _ => None,
            },

            _ => None,
        }
    }

    fn mk_grouped(&self) -> Vec<(u8, usize)> {
        let hm = self
            .0
            .iter()
            .map(|c| c.rank.value())
            .fold(HashMap::new(), |mut hm, value| {
                *hm.entry(value).or_insert(0_usize) += 1;
                hm
            });

        let mut v = hm.into_iter().collect::<Vec<_>>();
        v.sort_by(|a, b| b.1.cmp(&a.1).then_with(|| a.0.cmp(&b.0)));
        v
    }

    fn mk_value_array(&self) -> [u8; 5] {
        let values = self
            .0
            .iter()
            .rev()
            .map(|c| c.rank.value())
            .collect::<Vec<u8>>();

        let mut hand = [0; 5];
        hand.copy_from_slice(values.as_slice());
        hand
    }

    fn try_straight(&self) -> Option<PokerHand> {
        let values = self.0.iter().map(|c| c.rank.value()).collect::<Vec<_>>();

        let (max, sz, high) = if values[4] == Rank::Ace.value() {
            match values[0] {
                2 => (values[3], 4, 5),   // A,2,3,4,5 straight
                10 => (values[3], 4, 14), // 10,J,Q,K,A straight
                _ => return None,
            }
        } else {
            (values[4], 5, values[4])
        };

        let asc = values[0]..=max;
        if asc.len() == sz && values.iter().zip(asc).all(|(a, b)| *a == b) {
            Some(PokerHand::Straight(high))
        } else {
            None
        }
    }

    fn try_flush(&self) -> Option<PokerHand> {
        let cards = self.0;
        if cards.iter().all(|c| c.suit == cards[0].suit) {
            Some(PokerHand::Flush(self.mk_value_array()))
        } else {
            None
        }
    }

    // Checks for StraighFlush, Straight and Flush
    fn try_straight_flush(&self) -> Option<PokerHand> {
        use PokerHand::*;

        let straight = self.try_straight();
        let flush = self.try_flush();

        // if straight and flush, then unwrap and place into StraightFlush enum.
        // else return the appropriate true value.
        match straight {
            Some(Straight(m)) => match flush {
                Some(Flush(_)) => Some(StraightFlush(m)),
                _ => straight,
            },
            _ => flush,
        }
    }

    // Evaluates all possible PokerHands
    pub fn process(self) -> PokerHand {
        self.try_straight_flush()
            .or_else(|| Self::try_mult_cards(&self.mk_grouped()))
            .unwrap_or_else(|| PokerHand::HighCard(self.mk_value_array()))
    }
}

#[cfg(tests)]
mod tests {
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
