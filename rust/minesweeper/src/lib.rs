use std::char::from_digit;

struct Board<'a> {
    places: &'a [&'a str],
}

impl<'a> From<&'a [&'a str]> for Board<'a> {
    fn from(places: &'a [&'a str]) -> Self {
        Self { places }
    }
}

impl Board<'_> {
    fn annotate(self) -> Vec<String> {
        self.places
            .iter()
            .enumerate()
            .map(|(x, row)| {
                row.bytes()
                    .enumerate()
                    .map(|(y, p)| self.count_mines(p, x, y))
                    .collect()
            })
            .collect()
    }

    fn count_mines(&self, p: u8, r: usize, c: usize) -> char {
        match p {
            b'0'..=b'9' | b'*' | b'?' => p as char,
            b' ' => {
                let mine_count = self
                    .neighbors(r, c)
                    .filter(|&(row, col)| self.places[row].as_bytes()[col] == b'*')
                    .count();

                if mine_count == 0 {
                    ' '
                } else {
                    from_digit(mine_count as u32, 10).unwrap_or('?')
                }
            }
            _ => unreachable!("Unexpected character '{}'", p),
        }
    }

    fn get_range(low: usize, max: usize) -> impl Iterator<Item = usize> {
        let lower = low.saturating_sub(1);
        let upper = (low + 2).min(max);
        lower..upper
    }

    fn neighbors(&self, r: usize, c: usize) -> impl Iterator<Item = (usize, usize)> + '_ {
        Self::get_range(r, self.places.len()).flat_map(move |row| {
            Self::get_range(c, self.places[r].len()).map(move |col| (row, col))
        })
    }
}

pub fn annotate(minefield: &[&str]) -> Vec<String> {
    Board::from(minefield).annotate()
}
