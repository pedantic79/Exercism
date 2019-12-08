use std::char::from_digit;
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

impl From<Board> for Vec<String> {
    fn from(input: Board) -> Self {
        input.places.into_iter().map(String::from_iter).collect()
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
                    .filter(|(row, col)| self.places[*row][*col] == '*')
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

    fn neighbors(&self, r: usize, c: usize) -> impl Iterator<Item = (usize, usize)> + '_ {
        get_range(r, self.places.len())
            .flat_map(move |row| get_range(c, self.places[r].len()).map(move |col| (row, col)))
    }
}

pub fn annotate(minefield: &[&str]) -> Vec<String> {
    Board::from(minefield).annotate().into()
}

fn get_range(n: usize, n_max: usize) -> impl Iterator<Item = usize> {
    n.saturating_sub(1)..(n + 2).min(n_max)
}
