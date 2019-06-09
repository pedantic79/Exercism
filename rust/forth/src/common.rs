#[derive(Debug, PartialEq)]
pub enum Error {
    DivisionByZero,
    StackUnderflow,
    UnknownWord,
    InvalidWord,
}

pub type Value = i32;
pub type ForthResult = Result<(), Error>;

#[derive(Debug, Clone)]
pub enum Token {
    Number(Value),
    Identifier(String),
    Function(Box<Token>, Vec<Token>),
}
