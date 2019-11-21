use crate::{
    card::{Card, Rank},
    hand::pokerhand::PokerHand,
    Error,
};
use counter::Counter;
use std::convert::TryFrom;

/// FiveCards represents plain playing cards.
/// It has methods for evaluating the five cards, to determine the best PokerHand
pub struct FiveCards([Card; 5]);

impl TryFrom<&[Card]> for FiveCards {
    type Error = super::Error;

    fn try_from(cards: &[Card]) -> Result<Self, Self::Error> {
        if cards.len() != 5 {
            return Err(Error::InvalidCardCount);
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
            2 => match (sizes[0], sizes[1]) {
                (4, 1) => Some(FourKind(val[0], val[1])),
                (3, 2) => Some(FullHouse(val[0], val[1])),
                _ => None,
            },

            3 => match (sizes[0], sizes[1], sizes[2]) {
                (3, 1, 1) => Some(ThreeKind(val[0], [val[2], val[1]])),
                (2, 2, 1) => Some(TwoPair([val[1], val[0]], val[2])),
                _ => None,
            },

            4 => match (sizes[0], sizes[1], sizes[2], sizes[3]) {
                (2, 1, 1, 1) => Some(Pair(val[0], [val[3], val[2], val[1]])),
                _ => None,
            },

            _ => None,
        }
    }

    fn mk_grouped(&self) -> Vec<(u8, usize)> {
        self.0
            .iter()
            .map(|c| c.rank.value())
            .collect::<Counter<_>>()
            .most_common_ordered()
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
            .or_else(|| Some(PokerHand::HighCard(self.mk_value_array())))
            .unwrap()
    }
}
