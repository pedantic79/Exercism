use rayon::iter::ParallelIterator;
use rayon::slice::ParallelSlice;
use std::collections::HashMap;

pub fn frequency(input: &[&str], worker_count: usize) -> HashMap<char, usize> {
    if input.is_empty() || worker_count == 0 {
        return HashMap::new();
    }

    // Create a custom sized thread pool for each run
    let pool = rayon::ThreadPoolBuilder::new()
        .num_threads(worker_count)
        .build()
        .unwrap();

    let size = (input.len() + worker_count - 1) / worker_count;

    // override usage of global pool
    pool.install(|| {
        input
            .par_chunks(size)
            .map(count)
            .reduce(HashMap::new, |mut acc, value| {
                for (ch, count) in value {
                    acc.entry(ch).and_modify(|n| *n += count).or_insert(count);
                }
                acc
            })
    })
}

fn count(lines: &[&str]) -> HashMap<char, usize> {
    lines
        .iter()
        .flat_map(|line| line.chars())
        .filter(|c| c.is_alphabetic())
        .fold(HashMap::new(), |mut acc, c| {
            *acc.entry(c.to_lowercase().next().unwrap()).or_insert(0) += 1;
            acc
        })
}
