extern crate nom;

use nom::{
    branch::alt,
    bytes::complete::{is_a, tag},
    character::complete::{alphanumeric1, digit1, multispace0, multispace1},
    combinator::{all_consuming, map_opt, map_res},
    multi::{fold_many1, separated_list},
    sequence::{delimited, pair},
    IResult,
};

use std::collections::HashMap;

pub type Value = i32;
pub type ForthResult = Result<(), Error>;

#[derive(Default)]
pub struct Forth {
    stack: Vec<Value>,
    symbol: HashMap<String, Vec<Token>>,
}

#[derive(Debug, PartialEq)]
pub enum Error {
    DivisionByZero,
    StackUnderflow,
    UnknownWord,
    InvalidWord,
}

#[derive(Debug, Clone)]
pub enum BinaryOp {
    Multiply,
    Divide,
    Add,
    Subtract,
}

#[derive(Debug, Clone)]
pub enum Token {
    Number(Value),
    Operate(BinaryOp),
    Identifier(String),
    Function(Box<Token>, Vec<Token>),
}

fn integer(input: &str) -> IResult<&str, Token> {
    map_res(digit1, |i: &str| {
        Value::from_str_radix(i, 10).map(Token::Number)
    })(input)
}

fn operator(input: &str) -> IResult<&str, Token> {
    map_res(is_a("*/+-"), |i: &str| match i {
        "+" => Ok(Token::Operate(BinaryOp::Add)),
        "-" => Ok(Token::Operate(BinaryOp::Subtract)),
        "/" => Ok(Token::Operate(BinaryOp::Divide)),
        "*" => Ok(Token::Operate(BinaryOp::Multiply)),
        _ => Err(Error::UnknownWord),
    })(input)
}

fn to_identifier(i: String) -> Result<Token, Error> {
    Ok(Token::Identifier(i.to_lowercase()))
}

fn keyword(input: &str) -> IResult<&str, Token> {
    map_res(
        fold_many1(
            alt((alphanumeric1, tag("-"))),
            String::from(""),
            |acc, x| acc + x,
        ),
        to_identifier,
    )(input)
}

fn token(input: &str) -> IResult<&str, Token> {
    alt((integer, operator, keyword))(input)
}

pub fn function(input: &str) -> IResult<&str, Vec<Token>> {
    map_opt(
        delimited(
            pair(tag(":"), multispace0),
            separated_list(multispace1, token),
            pair(multispace0, tag(";")),
        ),
        |v: Vec<Token>| {
            if let Some((head, tail)) = v.split_first() {
                Some(vec![Token::Function(Box::new(head.clone()), tail.to_vec())])
            } else {
                None
            }
        },
    )(input)
}

pub fn token_list(input: &str) -> IResult<&str, Vec<Token>> {
    all_consuming(alt((function, separated_list(multispace1, token))))(input)
}

impl Forth {
    pub fn new() -> Self {
        Self {
            stack: Vec::new(),
            symbol: HashMap::new(),
        }
    }
    pub fn stack(&self) -> Vec<Value> {
        self.stack.clone()
    }

    fn do_tokens(&mut self, input: Vec<Token>) -> ForthResult {
        for t in input {
            match t {
                Token::Number(n) => self.stack.push(n),
                Token::Operate(o) => self.binary_op(o)?,
                Token::Identifier(i) => self.operation(i)?,
                Token::Function(name, body) => self.add_function(*name, body)?,
            }
        }
        Ok(())
    }

    fn get_top_two(&mut self) -> Result<(Value, Value), Error> {
        if let Some(r) = self.stack.pop() {
            if let Some(l) = self.stack.pop() {
                return Ok((l, r));
            }
        }
        Err(Error::StackUnderflow)
    }

    fn binary_op(&mut self, operator: BinaryOp) -> ForthResult {
        let (l, r) = self.get_top_two()?;
        match operator {
            BinaryOp::Add => {
                self.stack.push(l + r);
                Ok(())
            }
            BinaryOp::Subtract => {
                self.stack.push(l - r);
                Ok(())
            }
            BinaryOp::Multiply => {
                self.stack.push(l * r);
                Ok(())
            }
            BinaryOp::Divide => {
                if r == 0 {
                    Err(Error::DivisionByZero)
                } else {
                    self.stack.push(l / r);
                    Ok(())
                }
            }
        }
    }

    fn operation(&mut self, operator: String) -> ForthResult {
        let s = operator.as_ref();

        if let Some(ops) = self.symbol.get(s) {
            let v = ops.to_vec();
            self.do_tokens(v)
        } else {
            match s {
                "dup" => self.duplicate(),
                "drop" => self.drop(),
                "swap" => self.swap(),
                "over" => self.over(),
                _ => Err(Error::UnknownWord),
            }
        }
    }

    fn duplicate(&mut self) -> ForthResult {
        if let Some(&top) = self.stack.last() {
            self.stack.push(top);
            Ok(())
        } else {
            Err(Error::StackUnderflow)
        }
    }

    fn drop(&mut self) -> ForthResult {
        if self.stack.pop().is_some() {
            Ok(())
        } else {
            Err(Error::StackUnderflow)
        }
    }

    fn swap(&mut self) -> ForthResult {
        let (l, r) = self.get_top_two()?;
        self.stack.push(r);
        self.stack.push(l);
        Ok(())
    }

    fn over(&mut self) -> ForthResult {
        let (l, r) = self.get_top_two()?;
        self.stack.push(l);
        self.stack.push(r);
        self.stack.push(l);
        Ok(())
    }

    fn add_function(&mut self, name: Token, tokens: Vec<Token>) -> ForthResult {
        if let Token::Identifier(fname) = name {
            // if is_bin_op(fname.chars().next().unwrap()) {
            //     return Err(Error::InvalidWord);
            // }
            self.symbol.insert(fname, tokens);
            Ok(())
        } else {
            Err(Error::InvalidWord)
        }
    }

    pub fn eval(&mut self, input: &str) -> ForthResult {
        let f = token_list(input);
        match f {
            Ok((_, tokens)) => self.do_tokens(tokens),
            Err(_) => Err(Error::InvalidWord),
        }
    }
}
