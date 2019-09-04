mod poscursor;

pub struct CircularBuffer<T> {
    field: Vec<T>,
    read: poscursor::PosCursor,
    write: poscursor::PosCursor,
}

#[derive(Debug, PartialEq)]
pub enum Error {
    EmptyBuffer,
    FullBuffer,
}

impl<T: Clone + Default> CircularBuffer<T> {
    pub fn new(capacity: usize) -> Self {
        let capacity = capacity + 1;
        let field = vec![T::default(); capacity];
        Self {
            field,
            read: poscursor::PosCursor::new(capacity),
            write: poscursor::PosCursor::new(capacity),
        }
    }

    pub fn write(&mut self, element: T) -> Result<(), Error> {
        if self.is_full() {
            Err(Error::FullBuffer)
        } else {
            self.field[self.write.get()] = element;
            self.write.advance();
            Ok(())
        }
    }

    pub fn read(&mut self) -> Result<T, Error> {
        if self.read == self.write {
            Err(Error::EmptyBuffer)
        } else {
            let v = self.field[self.read.get()].clone();
            self.read.advance();
            Ok(v)
        }
    }

    pub fn clear(&mut self) {
        self.read.clear();
        self.write.clear();
    }

    pub fn overwrite(&mut self, element: T) {
        if self.is_full() {
            self.read.advance();
        }

        self.field[self.write.get()] = element;
        self.write.advance();
    }

    fn is_full(&self) -> bool {
        self.write.is_full(&self.read)
    }
}
