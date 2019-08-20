use std::char;
use std::iter::FromIterator;

struct Board {
    places: Vec<Vec<char>>,
}

impl From<&[&str]> for Board {
    fn from(input: &[&str]) -> Self {
        let places = input.iter().map(|&r| r.chars().collect()).collect();
        Self { places }
    }
}

impl Into<Vec<String>> for Board {
    fn into(self) -> Vec<String> {
        self.places.into_iter().map(String::from_iter).collect()
    }
}

impl Board {
    fn annotate(self) -> Self {
        let places = self
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

        Self { places }
    }

    fn count_mines(&self, r: usize, c: usize) -> char {
        let p = self.places[r][c];
        match p {
            '0'..='9' | '*' | '?' => p,
            ' ' => {
                let mine_count = self
                    .neighbors(r, c)
                    .into_iter()
                    .filter(|(row, col)| self.places[*row][*col] == '*')
                    .count();

                if mine_count == 0 {
                    ' '
                } else {
                    char::from_digit(mine_count as u32, 10).unwrap_or('?')
                }
            }
            _ => unreachable!("Unexpected character '{}'", p),
        }
    }

    fn neighbors(&self, r: usize, c: usize) -> Vec<(usize, usize)> {
        get_range(r, self.places.len())
            .flat_map(|row| get_range(c, self.places[r].len()).map(move |col| (row, col)))
            .collect()
    }
}

pub fn annotate(minefield: &[&str]) -> Vec<String> {
    let b: Board = minefield.into();
    b.annotate().into()
}

fn get_range(n: usize, n_max: usize) -> impl Iterator<Item = usize> {
    n.saturating_sub(1)..(n + 2).min(n_max)
}
