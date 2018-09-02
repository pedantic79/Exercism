extern crate counter;

use counter::Counter;
use std::cmp::Ordering;

pub fn winning_hands<'a>(hands: &[&'a str]) -> Option<Vec<&'a str>> {
    let mut h = hands
        .iter()
        .map(|h| Hand::try_from(h))
        .collect::<Option<Vec<Hand>>>()?;
    h.sort();

    let winner = h.last()?;
    let f = h
        .iter()
        .filter(|x| x.pokerhand == winner.pokerhand)
        .map(|x| x.source)
        .collect();

    Some(f)
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd)]
pub enum Suit {
    Clubs,
    Diamonds,
    Hearts,
    Spades,
}

impl Suit {
    pub fn try_from(s: &str) -> Option<Suit> {
        use Suit::*;
        match s {
            "S" => Some(Spades),
            "H" => Some(Hearts),
            "D" => Some(Diamonds),
            "C" => Some(Clubs),
            _ => None,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Rank {
    Num(u8),
    Jack,
    Queen,
    King,
    Ace,
}

impl Rank {
    pub fn try_from(s: &str) -> Option<Rank> {
        use Rank::*;
        match s {
            "A" => Some(Ace),
            "K" => Some(King),
            "Q" => Some(Queen),
            "J" => Some(Jack),
            "10" => Some(Num(10)),
            "9" => Some(Num(9)),
            "8" => Some(Num(8)),
            "7" => Some(Num(7)),
            "6" => Some(Num(6)),
            "5" => Some(Num(5)),
            "4" => Some(Num(4)),
            "3" => Some(Num(3)),
            "2" => Some(Num(2)),
            _ => None,
        }
    }

    fn value(self) -> u8 {
        use Rank::*;
        match self {
            Ace => 14,
            King => 13,
            Queen => 12,
            Jack => 11,
            Num(n) => n,
        }
    }
}

impl PartialOrd for Rank {
    fn partial_cmp(&self, other: &Rank) -> Option<Ordering> {
        Some(self.value().cmp(&other.value()))
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd)]
pub struct Card {
    rank: Rank,
    suit: Suit,
}

impl Card {
    pub fn new(r: Rank, s: Suit) -> Self {
        Card { rank: r, suit: s }
    }

    pub fn try_from(s: &str) -> Option<Self> {
        let l = s.len();

        if l == 2 || l == 3 {
            let split = (l + 1) / 2;
            let r = Rank::try_from(&s[..split])?;
            let t = Suit::try_from(&s[split..])?;

            Some(Card::new(r, t))
        } else {
            None
        }
    }
}

impl Ord for Card {
    fn cmp(&self, other: &Card) -> Ordering {
        self.partial_cmp(other).unwrap_or(Ordering::Less)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Hand<'a> {
    source: &'a str,
    pokerhand: PokerHand,
}

impl<'a> Hand<'a> {
    pub fn try_new(src: &'a str, cards: &[Card]) -> Option<Self> {
        if cards.len() != 5 {
            return None;
        }

        let mut c = cards.to_owned();
        c.sort();
        let cards = [c[0], c[1], c[2], c[3], c[4]];

        Some(Hand {
            source: src,
            pokerhand: PokerHand::process(&cards),
        })
    }

    pub fn try_from(s: &'a str) -> Option<Hand> {
        let cards = s
            .split_whitespace()
            .map(|card| Card::try_from(card))
            .collect::<Option<Vec<Card>>>()?;

        Hand::try_new(s, &cards)
    }
}

impl<'a> Ord for Hand<'a> {
    fn cmp(&self, other: &Hand) -> Ordering {
        self.pokerhand.cmp(&other.pokerhand)
    }
}

impl<'a> PartialOrd for Hand<'a> {
    fn partial_cmp(&self, other: &Hand) -> Option<Ordering> {
        self.pokerhand.partial_cmp(&other.pokerhand)
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub enum PokerHand {
    HighCard([u8; 5]),
    Pair(u8, [u8; 3]),
    TwoPair([u8; 2], u8),
    ThreeKind(u8, [u8; 2]),
    Straight(u8),
    Flush([u8; 5]),
    FullHouse(u8, u8),
    FourKind(u8, u8),
    StraightFlush(u8),
}

impl PokerHand {
    fn try_straight(cards: &[Card]) -> Option<PokerHand> {
        let values = cards.iter().map(|c| c.rank.value()).collect::<Vec<u8>>();

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
        if asc.len() == sz && values.iter().zip(values[0]..max).all(|(a, b)| *a == b) {
            Some(PokerHand::Straight(high))
        } else {
            None
        }
    }

    fn try_flush(cards: &[Card]) -> Option<PokerHand> {
        if cards.iter().all(|c| c.suit == cards[0].suit) {
            Some(PokerHand::Flush(PokerHand::mk_value_array(cards)))
        } else {
            None
        }
    }

    // Checks for StraighFlush, Straight and Flush
    fn try_straight_flush(cards: &[Card]) -> Option<PokerHand> {
        use PokerHand::*;

        let straight = PokerHand::try_straight(cards);
        let flush = PokerHand::try_flush(cards);

        match straight {
            Some(Straight(m)) => match flush {
                Some(Flush(_)) => Some(StraightFlush(m)),
                _ => straight,
            },
            _ => flush,
        }
    }

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

    fn mk_value_array(cards: &[Card]) -> [u8; 5] {
        let values = cards
            .iter()
            .rev()
            .map(|c| c.rank.value())
            .collect::<Vec<u8>>();

        let mut hand = [0; 5];
        hand.copy_from_slice(values.as_slice());
        hand
    }

    fn mk_grouped(cards: &[Card]) -> Vec<(u8, usize)> {
        cards
            .iter()
            .map(|c| c.rank.value())
            .collect::<Vec<u8>>()
            .iter()
            .cloned()
            .collect::<Counter<_>>()
            .most_common_ordered()
    }

    // Evaluates all possible PokerHands
    pub fn process(cards: &[Card]) -> PokerHand {
        PokerHand::try_straight_flush(cards)
            .or_else(|| PokerHand::try_mult_cards(&PokerHand::mk_grouped(cards)))
            .or_else(|| Some(PokerHand::HighCard(PokerHand::mk_value_array(cards))))
            .unwrap()
    }
}
