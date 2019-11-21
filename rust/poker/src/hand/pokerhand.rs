use super::FiveCards;

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

impl From<FiveCards> for PokerHand {
    fn from(cardhand: FiveCards) -> Self {
        cardhand.process()
    }
}
