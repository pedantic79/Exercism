use std::cmp::Ordering;
use std::fmt;

pub enum Outcome {
    Win,
    Lose,
    Draw,
}

#[derive(Clone)]
pub struct Record {
    name: String,
    wins: u8,
    lose: u8,
    draw: u8,
    points: u8,
}

impl Record {
    pub fn new(name: &str) -> Self {
        Record {
            wins: 0,
            lose: 0,
            draw: 0,
            points: 0,
            name: name.to_string(),
        }
    }

    pub fn outcome(&mut self, outcome: Outcome) {
        match outcome {
            Outcome::Win => {
                self.wins += 1;
                self.points += 3;
            }
            Outcome::Lose => self.lose += 1,
            Outcome::Draw => {
                self.draw += 1;
                self.points += 1;
            }
        };
    }
}

impl Ord for Record {
    fn cmp(&self, other: &Self) -> Ordering {
        self.points
            .cmp(&other.points)
            .then_with(|| other.name.cmp(&self.name))
    }
}

impl PartialOrd for Record {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(other.cmp(&self))
    }
}

impl PartialEq for Record {
    fn eq(&self, other: &Self) -> bool {
        self.points == other.points && self.name == other.name
    }
}

impl Eq for Record {}

impl fmt::Display for Record {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{:<30} | {:>2} | {:>2} | {:>2} | {:>2} | {:>2}",
            self.name,
            self.wins + self.lose + self.draw,
            self.wins,
            self.draw,
            self.lose,
            self.points
        )
    }
}
