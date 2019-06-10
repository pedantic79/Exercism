mod common;

mod parse;
mod stack;
pub use common::{Error, ForthResult, Token, Value};
pub use parse::token_list;
use stack::Stack;
use std::collections::HashMap;

#[derive(Clone)]
pub enum FnOps {
    Func(String),
    Add,
    Sub,
    Div,
    Mul,
    Swap,
    Dup,
    Drop,
    Over,
}

pub struct Forth {
    stack: Stack,
    symbol: HashMap<String, Vec<FnOps>>,
}

impl Default for Forth {
    fn default() -> Self {
        let mut builtin = HashMap::new();
        builtin.insert(String::from("*"), vec![FnOps::Mul]);
        builtin.insert(String::from("-"), vec![FnOps::Sub]);
        builtin.insert(String::from("+"), vec![FnOps::Add]);
        builtin.insert(String::from("/"), vec![FnOps::Div]);
        builtin.insert(String::from("swap"), vec![FnOps::Swap]);
        builtin.insert(String::from("dup"), vec![FnOps::Dup]);
        builtin.insert(String::from("drop"), vec![FnOps::Drop]);
        builtin.insert(String::from("over"), vec![FnOps::Over]);

        Self {
            stack: Stack::new(),
            symbol: builtin,
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

    fn do_tokens(&mut self, input: Vec<Token>) -> ForthResult {
        for t in input {
            match t {
                Token::Number(n) => self.stack.push(n),
                Token::Identifier(i) => self.operation(&i)?,
                Token::Function(name, body) => self.add_function(*name, body)?,
            }
        }
        Ok(())
    }

    fn operation(&mut self, operator: &str) -> ForthResult {
        let mut operations = Vec::new();
        if let Some(ops) = self.symbol.get(operator) {
            for o in ops {
                match o {
                    FnOps::Func(n) => {
                        if self.symbol.contains_key(n) {
                            operations.extend(self.symbol.get(n).unwrap())
                        } else {
                            operations.push(o)
                        }
                    }
                    _ => operations.push(o),
                }
            }
        }

        for o in operations {
            match o {
                FnOps::Func(_) => return Err(Error::UnknownWord)?,
                FnOps::Mul => self.stack.op_mul()?,
                FnOps::Add => self.stack.op_add()?,
                FnOps::Sub => self.stack.op_sub()?,
                FnOps::Div => self.stack.op_div()?,
                FnOps::Swap => self.stack.swap()?,
                FnOps::Dup => self.stack.duplicate()?,
                FnOps::Drop => self.stack.drop()?,
                FnOps::Over => self.stack.over()?,
            }
        }
        Ok(())

    }

    fn add_function(&mut self, name: Token, tokens: Vec<Token>) -> ForthResult {
        if let Token::Identifier(fname) = name {
            // if is_bin_op(fname.chars().next().unwrap()) {
            //     return Err(Error::InvalidWord);
            // }
            // self.symbol.insert(fname, tokens);
            Ok(())
        } else {
            Err(Error::InvalidWord)
        }
    }

    pub fn eval(&mut self, input: &str) -> ForthResult {
        let f = token_list(input);
        println!("{:?}", f);
        match f {
            Ok((_, tokens)) => self.do_tokens(tokens),
            Err(_) => Err(Error::InvalidWord),
        }
    }
}
