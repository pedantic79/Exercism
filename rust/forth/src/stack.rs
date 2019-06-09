use super::common::{Error, Value, ForthResult};

#[derive(Default)]
pub struct Stack {
    stack: Vec<Value>,
}

impl Stack {
    pub fn new() -> Self {
        Self { stack: Vec::new() }
    }

    pub fn get(&self) -> Vec<Value> {
        self.stack.clone()
    }

    pub fn push(&mut self, v: Value) {
        self.stack.push(v)
    }

    pub fn get_top_two(&mut self) -> Result<(Value, Value), Error> {
        if let Some(r) = self.stack.pop() {
            if let Some(l) = self.stack.pop() {
                return Ok((l, r));
            }
        }
        Err(Error::StackUnderflow)
    }

    pub fn duplicate(&mut self) -> ForthResult {
        if let Some(&top) = self.stack.last() {
            self.stack.push(top);
            Ok(())
        } else {
            Err(Error::StackUnderflow)
        }
    }

    pub fn drop(&mut self) -> ForthResult {
        if self.stack.pop().is_some() {
            Ok(())
        } else {
            Err(Error::StackUnderflow)
        }
    }

    pub fn swap(&mut self) -> ForthResult {
        let (l, r) = self.get_top_two()?;
        self.stack.push(r);
        self.stack.push(l);
        Ok(())
    }

    pub fn over(&mut self) -> ForthResult {
        let (l, r) = self.get_top_two()?;
        self.stack.push(l);
        self.stack.push(r);
        self.stack.push(l);
        Ok(())
    }

    pub fn op_add(&mut self) -> ForthResult {
        let (l, r) = self.get_top_two()?;
        self.stack.push(l + r);
        Ok(())
    }

    pub fn op_sub(&mut self) -> ForthResult {
        let (l, r) = self.get_top_two()?;
        self.stack.push(l - r);
        Ok(())
    }

    pub fn op_mul(&mut self) -> ForthResult {
        let (l, r) = self.get_top_two()?;
        self.stack.push(l * r);
        Ok(())
    }

    pub fn op_div(&mut self) -> ForthResult {
        let (l, r) = self.get_top_two()?;
        if r == 0 {
            Err(Error::DivisionByZero)
        } else {
            self.stack.push(l / r);
            Ok(())
        }
    }
}
