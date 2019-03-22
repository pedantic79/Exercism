use std::collections::HashMap;
use std::sync::mpsc::channel;
use std::thread;

pub fn frequency(input: &[&str], worker_count: usize) -> HashMap<char, usize> {
    let worker_inputs = split_input(&input, worker_count);
    let (tx, rx) = channel();

    for worker_input in worker_inputs {
        let tx = tx.clone();
        thread::spawn(move || tx.send(count(&worker_input)).unwrap());
    }

    let mut results = HashMap::new();
    for _ in 0..worker_count {
        let worker_result = rx.recv().unwrap();
        for (character, count) in worker_result {
            *results.entry(character).or_insert(0) += count;
        }
    }
    results
}

fn split_input(input: &[&str], count: usize) -> Vec<Vec<String>> {
    let mut result: Vec<Vec<String>> = Vec::with_capacity(count);

    // Ceil with only integral math
    let size = (input.len() + count - 1) / count;
    for _ in 0..count {
        result.push(Vec::with_capacity(size));
    }

    for (i, line) in input.iter().enumerate() {
        let index = i % count;
        result[index].push(line.to_string());
    }

    result
}

fn count(lines: &[String]) -> HashMap<char, usize> {
    let mut results = HashMap::new();

    for line in lines {
        for c in line.chars().filter(|x| x.is_alphabetic()) {
            *results.entry(c.to_lowercase().next().unwrap()).or_insert(0) += 1;
        }
    }

    results
}
