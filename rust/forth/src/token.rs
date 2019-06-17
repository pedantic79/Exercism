use super::{Error, Value};

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Token {
    Num(Value),
    Word(String),
    Add,
    Sub,
    Mul,
    Div,
    Dup,
    Drop,
    Swap,
    Over,
    WordStart,
    WordEnd,
}

impl std::str::FromStr for Token {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let token = if let Ok(val) = s.parse::<Value>() {
            Token::Num(val)
        } else {
            match s {
                "+" => Token::Add,
                "-" => Token::Sub,
                "*" => Token::Mul,
                "/" => Token::Div,

                "DUP" => Token::Dup,
                "DROP" => Token::Drop,
                "SWAP" => Token::Swap,
                "OVER" => Token::Over,

                ":" => Token::WordStart,
                ";" => Token::WordEnd,
                _ => Token::Word(s.to_string()),
            }
        };
        Ok(token)
    }
}
