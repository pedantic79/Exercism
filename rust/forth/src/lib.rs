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

    pub fn eval(&mut self, input: &str) -> ForthResult {
        // Parse input into statements, properly handling definitions (: ... ;) and executions
        let tokens: Vec<Token> = input
            .split_whitespace()
            .map(|s| s.to_uppercase().parse::<Token>().unwrap())
            .collect();

        let mut idx = 0;
        while idx < tokens.len() {
            if tokens[idx] == Token::WordStart {
                // This is a definition; find the matching semicolon
                let start = idx;
                let mut depth = 1;
                idx += 1;
                while idx < tokens.len() && depth > 0 {
                    if tokens[idx] == Token::WordStart {
                        depth += 1;
                    } else if tokens[idx] == Token::WordEnd {
                        depth -= 1;
                    }
                    idx += 1;
                }
                let statement = tokens[start..idx].to_vec();
                self.add_func(statement)?;
            } else {
                // This is an execution; collect tokens until a WordStart or end of input
                let start = idx;
                while idx < tokens.len() && tokens[idx] != Token::WordStart {
                    idx += 1;
                }
                let statement = tokens[start..idx].to_vec();
                if !statement.is_empty() {
                    self.execute_tokens(statement)?;
                }
            }
        }

        Ok(())
    }

    fn add_func(&mut self, tokens: Vec<Token>) -> ForthResult {
        let len = tokens.len();

        if len > 3 && tokens[0] == Token::WordStart && tokens[len - 1] == Token::WordEnd {
            let name = &tokens[1];
            if let Token::Num(_) = name {
                Err(Error::InvalidWord)
            } else if *name == Token::WordStart || *name == Token::WordEnd {
                Err(Error::InvalidWord)
            } else {
                // Try to expand custom words at definition time to capture definitions,
                // but limit expansion to prevent exponential growth (alloc-attack)
                let definition_tokens = &tokens[2..len - 1];
                let expanded = self.expand_once(definition_tokens);

                // Only use the expansion if it's not too large (to prevent alloc-attack)
                // Cap at 10000 tokens to allow normal use while preventing exponential growth
                if expanded.len() <= 10000 {
                    self.words.insert(name.clone(), expanded);
                } else {
                    // Store unexpanded version for very large expansions
                    self.words.insert(name.clone(), definition_tokens.to_vec());
                }
                Ok(())
            }
        } else {
            Err(Error::InvalidWord)
        }
    }

    fn execute_tokens(&mut self, tokens: Vec<Token>) -> ForthResult {
        // Expand custom words iteratively until no more custom words remain
        let mut current = tokens;
        const MAX_EXPANSIONS: usize = 1000;

        for _ in 0..MAX_EXPANSIONS {
            let expanded = self.expand_once(&current);

            // Check if any custom words remain
            let has_custom_words = expanded.iter().any(|t| matches!(t, Token::Word(_)));

            current = expanded;

            if !has_custom_words {
                break;
            }
        }

        for token in current {
            match token {
                Token::Num(n) => self.stack.push(n),
                Token::Add => self.stack.op_add()?,
                Token::Sub => self.stack.op_sub()?,
                Token::Mul => self.stack.op_mul()?,
                Token::Div => self.stack.op_div()?,
                Token::Dup => self.stack.op_dup()?,
                Token::Drop => self.stack.op_drop()?,
                Token::Swap => self.stack.op_swap()?,
                Token::Over => self.stack.op_over()?,
                Token::WordEnd | Token::WordStart => return Err(Error::InvalidWord),
                Token::Word(_) => return Err(Error::UnknownWord),
            }
        }
        Ok(())
    }

    fn expand_once(&self, tokens: &[Token]) -> Vec<Token> {
        let mut expanded = Vec::new();
        for token in tokens.iter() {
            if let Some(definition) = self.words.get(token) {
                expanded.extend(definition.iter().cloned());
            } else {
                expanded.push(token.clone());
            }
        }
        expanded
    }
}
