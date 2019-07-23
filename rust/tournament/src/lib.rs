mod record;

use record::{Outcome, Record};
use std::collections::{BinaryHeap, HashMap};
use std::iter::once;

pub fn tally(match_results: &str) -> String {
    let records = if match_results.is_empty() {
        Vec::new()
    } else {
        match_results
            .split('\n')
            .flat_map(|line| {
                let t = line.split(';').collect::<Vec<_>>();
                assert_eq!(t.len(), 3, "invalid input line: [{}]", line);

                match t[2].trim() {
                    "win" => mk_iter(t[0], Outcome::Win, t[1], Outcome::Lose),
                    "loss" => mk_iter(t[1], Outcome::Win, t[0], Outcome::Lose),
                    "draw" => mk_iter(t[0], Outcome::Draw, t[1], Outcome::Draw),
                    other => unreachable!("invalid outcome: [{}]", other),
                }
            })
            .fold(HashMap::new(), |mut hm, (name, outcome)| {
                hm.entry(name).or_insert_with(|| Record::new(name)).outcome(outcome);
                hm
            })
            .values()
            .cloned()
            .collect::<BinaryHeap<_>>()
            .into_sorted_vec()
    };

    once(format!("{:<30} | MP |  W |  D |  L |  P", "Team"))
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
