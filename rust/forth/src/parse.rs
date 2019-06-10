use nom::{
    branch::alt,
    bytes::complete::{is_a, tag},
    character::complete::{alphanumeric1, digit1, multispace0, multispace1},
    combinator::{all_consuming, map_opt, map_res},
    multi::{fold_many1, separated_list},
    sequence::{delimited, pair},
    IResult,
};

use super::common::{Error, Token, Value};

fn integer(input: &str) -> IResult<&str, Token> {
    map_res(digit1, |i: &str| {
        Value::from_str_radix(i, 10).map(Token::Number)
    })(input)
}

fn operator(input: &str) -> IResult<&str, Token> {
    map_res(is_a("*/+-"), |i: &str| -> Result<Token, Error> {
        Ok(Token::Identifier(i.to_string()))
    })(input)
}

fn identifier(input: &str) -> IResult<&str, Token> {
    map_res(
        fold_many1(
            alt((alphanumeric1, tag("-"))),
            String::from(""),
            |acc, x| acc + x,
        ),
        |i: String| -> Result<Token, Error> { Ok(Token::Identifier(i.to_lowercase())) },
    )(input)
}

fn token(input: &str) -> IResult<&str, Token> {
    alt((integer, operator, identifier))(input)
}

fn function(input: &str) -> IResult<&str, Vec<Token>> {
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
