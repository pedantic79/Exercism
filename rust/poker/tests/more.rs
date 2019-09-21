use maplit::hashmap;
use poker::{Card, Error, Hand, PokerHand, Rank, Suit};
use std::collections::HashMap;
use std::convert::TryFrom;

fn suit_map<'a>() -> HashMap<&'a str, Suit> {
    use Suit::*;

    hashmap!(
        "D" => Diamonds,
        "C" => Clubs,
        "H" => Hearts,
        "S" => Spades,
    )
}

fn rank_map<'a>() -> HashMap<&'a str, Rank> {
    use Rank::*;

    hashmap!(
        "A" => Ace,
        "K" => King,
        "Q" => Queen,
        "J" => Jack,
        "10" => Num(10),
        "9" => Num(9),
        "8" => Num(8),
        "7" => Num(7),
        "6" => Num(6),
        "5" => Num(5),
        "4" => Num(4),
        "3" => Num(3),
        "2" => Num(2),
    )
}

#[test]
fn test_parse_suit() {
    suit_map()
        .iter()
        .for_each(|(&k, v)| assert_eq!(Suit::try_from(k).as_ref(), Ok(v)));

    assert_eq!(Suit::try_from("A"), Err(Error::InvalidSuit));
}

#[test]
fn test_parse_rank() {
    rank_map()
        .iter()
        .for_each(|(&k, v)| assert_eq!(Rank::try_from(k).as_ref(), Ok(v)));

    assert_eq!(Rank::try_from("1"), Err(Error::InvalidRank));
}

#[test]
fn test_parse_card() {
    rank_map().iter().for_each(|(r, rank_exp)| {
        suit_map().iter().for_each(|(s, suit_exp)| {
            let card = format!("{}{}", r, s);
            let exp = poker::Card::new(*rank_exp, *suit_exp);
            assert_eq!(Card::try_from(&card[..]), Ok(exp));
        })
    });

    assert_eq!(Card::try_from("1S"), Err(Error::InvalidRank));
    assert_eq!(Card::try_from("AE"), Err(Error::InvalidSuit));
}

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
        PokerHand::process(mk_hand(["3S", "4D", "5C", "6H", "7H"]).as_ref()),
        Straight(7)
    );
    assert_eq!(
        PokerHand::process(mk_hand(["AS", "2D", "3C", "4H", "5H"]).as_ref()),
        Straight(5)
    );
    assert_eq!(
        PokerHand::process(mk_hand(["KS", "AD", "2C", "3H", "4H"]).as_ref()),
        HighCard([14, 13, 4, 3, 2])
    );
    assert_eq!(
        PokerHand::process(mk_hand(["KS", "QD", "10C", "JH", "AH"]).as_ref()),
        Straight(14)
    )
}

#[test]
fn test_is_flush() {
    use PokerHand::*;
    assert_eq!(
        PokerHand::process(mk_hand(["3S", "4D", "5C", "6H", "7H"]).as_ref()),
        Straight(7)
    );
    assert_eq!(
        PokerHand::process(mk_hand(["4H", "2H", "3H", "9H", "5H"]).as_ref()),
        Flush([9, 5, 4, 3, 2])
    );
    assert_eq!(
        PokerHand::process(mk_hand(["4H", "AH", "3H", "6H", "5H"]).as_ref()),
        Flush([14, 6, 5, 4, 3])
    );
    assert_eq!(
        PokerHand::process(mk_hand(["3H", "QH", "10C", "JH", "AH"]).as_ref()),
        HighCard([14, 12, 11, 10, 3])
    )
}

#[test]
fn test_is_sflush() {
    use PokerHand::*;
    assert_eq!(
        PokerHand::process(mk_hand(["3S", "4D", "5C", "6H", "8D"]).as_ref()),
        HighCard([8, 6, 5, 4, 3])
    );
    assert_eq!(
        PokerHand::process(mk_hand(["4H", "2H", "3H", "9H", "5H"]).as_ref()),
        Flush([9, 5, 4, 3, 2])
    );
    assert_eq!(
        PokerHand::process(mk_hand(["4H", "2H", "3H", "6H", "5H"]).as_ref()),
        StraightFlush(6)
    );
    assert_eq!(
        PokerHand::process(mk_hand(["KH", "QH", "10C", "JH", "AH"]).as_ref()),
        Straight(14)
    )
}

#[test]
fn test_the_rest() {
    use PokerHand::*;
    assert_eq!(
        PokerHand::process(mk_hand(["3S", "3D", "3C", "4H", "4H"]).as_ref()),
        FullHouse(3, 4)
    );
    assert_eq!(
        PokerHand::process(mk_hand(["3S", "3D", "4C", "4H", "4H"]).as_ref()),
        FullHouse(4, 3)
    );
    assert_eq!(
        PokerHand::process(mk_hand(["3S", "3D", "AC", "4H", "4H"]).as_ref()),
        TwoPair([4, 3], 14)
    );
    assert_eq!(
        PokerHand::process(mk_hand(["3S", "3D", "AC", "KH", "10H"]).as_ref()),
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

fn mk_hand(cards: [&str; 5]) -> Vec<Card> {
    let mut v = cards.iter().map(|c| mk_card(c)).collect::<Vec<Card>>();
    v.sort();
    v
}
