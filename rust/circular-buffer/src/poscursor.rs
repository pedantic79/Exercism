#[derive(PartialEq)]
pub struct PosCursor {
    max: usize,
    pos: usize,
}

impl PosCursor {
    pub fn new(max: usize) -> Self {
        PosCursor { max, pos: 0 }
    }

    pub fn advance(&mut self) {
        self.pos = (self.pos + 1) % self.max;
    }

    pub fn get(&self) -> usize {
        self.pos
    }

    pub fn clear(&mut self) {
        self.pos = 0;
    }

    pub fn is_full(&self, other: &Self) -> bool {
        (self.pos + 1) % self.max == other.pos
    }
}
