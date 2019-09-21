use counter::Counter;
use std::cmp::Ordering;
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd)]
pub enum Suit {
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
            _ => Err(Error::InvalidSuit),
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

impl TryFrom<&str> for Rank {
    type Error = Error;
    fn try_from(s: &str) -> Result<Self, Self::Error> {
        use Rank::*;
        match s {
            "A" => Ok(Ace),
            "K" => Ok(King),
            "Q" => Ok(Queen),
            "J" => Ok(Jack),
            "10" => Ok(Num(10)),
            "9" => Ok(Num(9)),
            "8" => Ok(Num(8)),
            "7" => Ok(Num(7)),
            "6" => Ok(Num(6)),
            "5" => Ok(Num(5)),
            "4" => Ok(Num(4)),
            "3" => Ok(Num(3)),
            "2" => Ok(Num(2)),
            _ => Err(Error::InvalidRank),
        }
    }
}

impl PartialOrd for Rank {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
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
            Err(Error::InvalidCard)
        }
    }
}

impl Ord for Card {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap_or(Ordering::Less)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Hand<'a> {
    source: &'a str,
    pokerhand: PokerHand,
}

impl<'a> Hand<'a> {
    pub fn try_new(src: &'a str, cards: &[Card]) -> Result<Self, Error> {
        if cards.len() != 5 {
            return Err(Error::InvalidCardCount);
        }

        let mut c = cards.to_owned();
        c.sort();
        let cards = [c[0], c[1], c[2], c[3], c[4]];

        Ok(Self {
            source: src,
            pokerhand: PokerHand::process(&cards),
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
    fn try_straight(cards: &[Card]) -> Option<Self> {
        let values = cards.iter().map(|c| c.rank.value()).collect::<Vec<_>>();

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

    fn try_flush(cards: &[Card]) -> Option<Self> {
        if cards.iter().all(|c| c.suit == cards[0].suit) {
            Some(PokerHand::Flush(Self::mk_value_array(cards)))
        } else {
            None
        }
    }

    // Checks for StraighFlush, Straight and Flush
    fn try_straight_flush(cards: &[Card]) -> Option<Self> {
        use PokerHand::*;

        let straight = Self::try_straight(cards);
        let flush = Self::try_flush(cards);

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

    // Checks for 4-kind, fullhouse, 3-kind, etc.
    fn try_mult_cards(sorted: &[(u8, usize)]) -> Option<Self> {
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
            .collect::<Counter<_>>()
            .most_common_ordered()
    }

    // Evaluates all possible PokerHands
    pub fn process(cards: &[Card]) -> Self {
        Self::try_straight_flush(cards)
            .or_else(|| Self::try_mult_cards(&Self::mk_grouped(cards)))
            .or_else(|| Some(PokerHand::HighCard(Self::mk_value_array(cards))))
            .unwrap()
    }
}
