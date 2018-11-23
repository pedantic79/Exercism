use std::char;

struct Board {
    places: Vec<Vec<char>>,
}

impl<'a> From<&'a [&'a str]> for Board {
    fn from(input: &[&str]) -> Board {
        let v = input
            .iter()
            .map(|&r| r.chars().collect())
            .collect();
        Board { places: v }
    }
}

impl Into<Vec<String>> for Board {
    fn into(self) -> Vec<String> {
        self.places
            .iter()
            .map(|r| r.iter().collect())
            .collect()
    }
}

impl Board {
    fn annotate(&mut self) {
        self.places = (0..self.places.len())
            .map(|x| {
                (0..self.places[x].len())
                    .map(|y| self.count_mines(x, y))
                    .collect()
            }).collect();
    }

    fn count_mines(&self, r: usize, c: usize) -> char {
        match self.places[r][c] {
            '*' => '*',
            _ => {
                let mine_count = self
                    .neighbors(r, c)
                    .iter()
                    .filter(|(row, col)| self.places[*row][*col] == '*')
                    .count();

                if mine_count == 0 {
                    ' '
                } else {
                    char::from_digit(mine_count as u32, 10).unwrap_or('?')
                }
            }
        }
    }

    fn neighbors(&self, r: usize, c: usize) -> Vec<(usize, usize)> {
        let row_max = self.places.len() - 1;
        let col_max = self.places[0].len() - 1;

        let min_r = if r < 1 { 0 } else { r - 1 };
        let max_r = if r == row_max { row_max } else { r + 1 };

        let min_c = if c < 1 { 0 } else { c - 1 };
        let max_c = if c == col_max { col_max } else { c + 1 };

        let mut v = Vec::new();
        (min_r..=max_r).for_each(|row| (min_c..=max_c).for_each(|col| v.push((row, col))));

        v
    }
}

pub fn annotate(minefield: &[&str]) -> Vec<String> {
    let mut b = Board::from(minefield);
    b.annotate();
    b.into()
}
