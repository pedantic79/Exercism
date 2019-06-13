mod stack;
mod token;

use std::collections::HashMap;

pub type Value = i32;
pub type ForthResult = Result<(), Error>;
use stack::Stack;
use token::Token;

pub struct Forth {
    stack: Stack,
    words: HashMap<Token, Vec<Token>>,
}

#[derive(Debug, PartialEq)]
pub enum Error {
    DivisionByZero,
    StackUnderflow,
    UnknownWord,
    InvalidWord,
}

impl Default for Forth {
    fn default() -> Self {
        Self {
            stack: Stack::new(),
            words: HashMap::new(),
        }
    }
}

impl Forth {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn stack(&self) -> Vec<Value> {
        self.stack.get()
    }

    fn add_func(&mut self, tokens: Vec<Token>) -> ForthResult {
        let len = tokens.len();

        if len > 3 && tokens[0] == Token::WordStart && tokens[len - 1] == Token::WordEnd {
            let name = &tokens[1];
            if let Token::Num(_) = name {
                Err(Error::InvalidWord)
            } else if *name == Token::WordStart || *name == Token::WordEnd  {
                Err(Error::InvalidWord)
            } else {
                self.words.insert(name.clone(), Vec::from(&tokens[2..len-1]));
                Ok(())
            }
        } else {
            Err(Error::InvalidWord)
        }

    }

    pub fn eval(&mut self, input: &str) -> ForthResult {
        let tokens: Vec<Token> = input
            .split_whitespace()
            .map(|s| s.to_uppercase().parse::<Token>().unwrap())
            .collect();

        if tokens.len() > 1 && tokens[0] == Token::WordStart {
            self.add_func(tokens)
        } else {
            Err(Error::UnknownWord)
        }

    }
}
