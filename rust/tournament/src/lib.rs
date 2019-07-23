use std::cmp::Ordering;
use std::collections::{BinaryHeap, HashMap};
use std::fmt;
use std::iter::once;

enum Outcome {
    Win,
    Lose,
    Draw,
}

#[derive(Clone)]
struct Record {
    name: String,
    wins: u8,
    lose: u8,
    draw: u8,
    points: u8,
}

impl Record {
    fn new(name: &str) -> Self {
        Record {
            wins: 0,
            lose: 0,
            draw: 0,
            points: 0,
            name: name.to_string(),
        }
    }

    fn wins(&mut self) {
        self.wins += 1;
        self.points += 3;
    }
    fn lose(&mut self) {
        self.lose += 1;
    }
    fn draw(&mut self) {
        self.draw += 1;
        self.points += 1;
    }
}

impl Ord for Record {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.points.cmp(&other.points) {
            Ordering::Equal => other.name.cmp(&self.name),
            order => order,
        }
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

pub fn tally(match_results: &str) -> String {
    let records = if match_results.is_empty() {
        Vec::new()
    } else {
        match_results
            .split('\n')
            .flat_map(|line| {
                let l = line.split(';').collect::<Vec<_>>();
                assert_eq!(l.len(), 3, "invalid input line: [{}]", line);

                match l[2].trim() {
                    "win" => mk_iter(l[0], Outcome::Win, l[1], Outcome::Lose),
                    "loss" => mk_iter(l[1], Outcome::Win, l[0], Outcome::Lose),
                    "draw" => mk_iter(l[0], Outcome::Draw, l[1], Outcome::Draw),
                    s => unreachable!("invalid outcome: [{}]", s),
                }
            })
            .fold(HashMap::new(), |mut hm, (name, outcome)| {
                let record = hm.entry(name).or_insert_with(|| Record::new(name));
                match outcome {
                    Outcome::Win => record.wins(),
                    Outcome::Lose => record.lose(),
                    Outcome::Draw => record.draw(),
                }
                hm
            })
            .values()
            .cloned()
            .collect::<BinaryHeap<_>>()
            .into_sorted_vec()
    };

    once("Team                           | MP |  W |  D |  L |  P".to_string())
        .chain(records.iter().map(|r| r.to_string()))
        .collect::<Vec<_>>()
        .join("\n")
}

fn mk_iter<'a>(
    team1: &'a str,
    out1: Outcome,
    team2: &'a str,
    out2: Outcome,
) -> impl Iterator<Item = (&'a str, Outcome)> {
    once((team1, out1)).chain(once((team2, out2)))
}
