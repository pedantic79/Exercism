#[derive(Debug)]
pub struct ChessPosition {
    rank: i32,
    file: i32,
}

#[derive(Debug)]
pub struct Queen(ChessPosition);

impl ChessPosition {
    pub fn new(rank: i32, file: i32) -> Option<Self> {
        if rank >= 0 && rank < 8 && file >= 0 && file < 8 {
            Some(Self { rank, file })
        } else {
            None
        }
    }
}

impl Queen {
    pub fn new(position: ChessPosition) -> Self {
        Self(position)
    }

    pub fn can_attack(&self, other: &Queen) -> bool {
        self.0.rank == other.0.rank
            || self.0.file == other.0.file
            || Self::diagonals(self.0.rank, self.0.file).contains(&(other.0.rank, other.0.file))
    }

    fn diagonals(rank: i32, file: i32) -> Vec<(i32, i32)> {
        let r_top = rank + 1..8;
        let r_bot = (0..rank).rev();
        let f_top = file + 1..8;
        let f_bot = (0..file).rev();

        // Is there a way to hold all of these in one array?
        let a = r_top.clone().zip(f_top.clone());
        let b = r_bot.clone().zip(f_bot.clone());
        let c = r_top.zip(f_bot);
        let d = r_bot.zip(f_top);

        a.chain(b).chain(c).chain(d).collect()
    }
}
