use super::{Error, ForthResult, Value};

pub struct Stack {
    stack: Vec<Value>,
}

pub type ValueResult = Result<Value, Error>;

impl Stack {
    pub fn new() -> Self {
        Self { stack: Vec::new() }
    }

    pub fn get(&self) -> Vec<Value> {
        self.stack.clone()
    }

    pub fn push(&mut self, v: Value) {
        self.stack.push(v);
    }

    fn stack_pop(&mut self) -> ValueResult {
        self.stack.pop().ok_or(Error::StackUnderflow)
    }

    fn stack_peek(&self, place: usize) -> ValueResult {
        if self.stack.len() > (place - 1) {
            Ok(self.stack[self.stack.len() - place])
        } else {
            Err(Error::StackUnderflow)
        }
    }

    pub fn op_dup(&mut self) -> ForthResult {
        let a = self.stack_peek(1)?;
        self.stack.push(a);
        Ok(())
    }

    pub fn op_drop(&mut self) -> ForthResult {
        self.stack_pop()?;

        Ok(())
    }

    pub fn op_swap(&mut self) -> ForthResult {
        let a = self.stack_pop()?;
        let b = self.stack_pop()?;

        self.stack.push(a);
        self.stack.push(b);
        Ok(())
    }

    pub fn op_over(&mut self) -> ForthResult {
        let a = self.stack_peek(2)?;

        self.stack.push(a);
        Ok(())
    }

    fn binary_op<F>(&mut self, f: F) -> ForthResult
    where
        F: (Fn(Value, Value) -> ValueResult),
    {
        let a = self.stack_pop()?;
        let b = self.stack_pop()?;

        let n = f(a, b)?;
        self.stack.push(n);
        Ok(())
    }

    pub fn op_add(&mut self) -> ForthResult {
        self.binary_op(|a, b| Ok(a + b))
    }

    pub fn op_sub(&mut self) -> ForthResult {
        self.binary_op(|a, b| Ok(b - a))
    }

    pub fn op_mul(&mut self) -> ForthResult {
        self.binary_op(|a, b| Ok(b * a))
    }

    pub fn op_div(&mut self) -> ForthResult {
        self.binary_op(|a, b| b.checked_div(a).ok_or(Error::DivisionByZero))
    }
}
