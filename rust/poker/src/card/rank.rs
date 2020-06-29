use crate::Error;
use std::convert::TryFrom;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum Rank {
    Num(u8),
    Jack,
    Queen,
    King,
    Ace,
}

impl Rank {
    pub fn value(self) -> u8 {
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
            _ => Err(Error::Rank),
        }
    }
}

#[cfg(tests)]
mod tests {
    use super::Rank;
    use maplit::hashmap;
    use std::collections::HashMap;

    fn rank_map() -> HashMap<&str, Rank> {
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
    fn test_parse_rank() {
        rank_map()
            .iter()
            .for_each(|(&k, v)| assert_eq!(Rank::try_from(k).as_ref(), Ok(v)));

        assert_eq!(Rank::try_from("1"), Err(Error::InvalidRank));
    }
}
