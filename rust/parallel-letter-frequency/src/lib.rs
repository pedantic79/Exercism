// use rayon::iter::ParallelIterator;
// use rayon::slice::ParallelSlice;
use crossbeam_utils::thread;
use std::collections::HashMap;

pub fn frequency(input: &[&str], worker_count: usize) -> HashMap<char, usize> {
    if input.is_empty() || worker_count == 0 {
        return HashMap::new();
    }

    let size = (input.len() + worker_count - 1) / worker_count;

    // override usage of global pool
    // pool.install(|| {
    // input
    //     .par_chunks(size)
    //     .map(count)
    //     .reduce(HashMap::new, |mut acc, value| {
    //         for (ch, count) in value {
    //             acc.entry(ch).and_modify(|n| *n += count).or_insert(count);
    //         }
    //         acc
    //     })
    // })

    if size < 5 {
        return count(input);
    }

    thread::scope(|scope| {
        let thread_handles = input
            .chunks(size)
            .map(|chunk| scope.spawn(move |_| count(chunk)))
            .collect::<Vec<_>>();

        thread_handles
            .into_iter()
            .fold(HashMap::new(), |mut acc, handle| {
                let hm = handle.join().unwrap();
                for (ch, count) in hm {
                    acc.entry(ch).and_modify(|n| *n += count).or_insert(count);
                }
                acc
            })
    })
    .unwrap()
}

fn count(texts: &[&str]) -> HashMap<char, usize> {
    let mut map = HashMap::new();

    for line in texts {
        for chr in line.chars().filter(|c| c.is_alphabetic()) {
            if let Some(c) = chr.to_lowercase().next() {
                (*map.entry(c).or_insert(0)) += 1;
            }
        }
    }

    map
}
