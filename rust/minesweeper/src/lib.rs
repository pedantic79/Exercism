use std::char;

struct Board {
    places: Vec<Vec<char>>,
}

impl<'a> From<&'a [&'a str]> for Board {
    fn from(input: &[&str]) -> Self {
        let v = input.iter().map(|&r| r.chars().collect()).collect();
        Self { places: v }
    }
}

impl Into<Vec<String>> for Board {
    fn into(self) -> Vec<String> {
        self.places.iter().map(|r| r.iter().collect()).collect()
    }
}

impl Board {
    fn annotate(&self) -> Self {
        let v = self
            .places
            .iter()
            .enumerate()
            .map(|(x, row)| {
                row.iter()
                    .enumerate()
                    .map(|(y, _)| self.count_mines(x, y))
                    .collect()
            })
            .collect();

        Self { places: v }
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
        let (min_r, max_r) = get_range(r, self.places.len());
        let (min_c, max_c) = get_range(c, self.places[r].len());
        let mut v = Vec::new();

        for row in min_r..=max_r {
            for col in min_c..=max_c {
                v.push((row, col));
            }
        }

        v
    }
}

pub fn annotate(minefield: &[&str]) -> Vec<String> {
    Board::from(minefield).annotate().into()
}

fn get_range(n: usize, n_max: usize) -> (usize, usize) {
    let min = if n == 0 { 0 } else { n - 1 };
    let max = if n == n_max - 1 { n_max - 1 } else { n + 1 };
    (min, max)
}
