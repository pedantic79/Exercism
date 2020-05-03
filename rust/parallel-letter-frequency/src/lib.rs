use std::collections::HashMap;

#[cfg(feature = "pariter")]
pub use crate::rayon::frequency;

#[cfg(feature = "crossbeam")]
pub use crate::crossbeam::frequency;

#[cfg(feature = "default")]
pub use crate::stdthread::frequency;

fn count<S: AsRef<str>>(texts: &[S]) -> HashMap<char, usize> {
    let mut map = HashMap::new();

    for line in texts.iter() {
        for chr in line.as_ref().chars().filter(|c| c.is_alphabetic()) {
            if let Some(c) = chr.to_lowercase().next() {
                (*map.entry(c).or_insert(0)) += 1;
            }
        }
    }

    map
}

fn calc_size(len: usize, worker_count: usize) -> usize {
    (len + worker_count - 1) / worker_count
}

#[cfg(feature = "pariter")]
mod rayon {
    use super::{calc_size, count};
    use rayon::iter::ParallelIterator;
    use rayon::slice::ParallelSlice;
    use std::collections::HashMap;

    #[allow(dead_code)]
    pub fn frequency(input: &[&str], worker_count: usize) -> HashMap<char, usize> {
        if input.is_empty() || worker_count == 0 {
            return HashMap::new();
        }

        let size = calc_size(input.len(), worker_count);
        if size < 5 {
            return count(input);
        }

        // Create a custom sized thread pool for each run
        let pool = rayon::ThreadPoolBuilder::new()
            .num_threads(worker_count)
            .build()
            .unwrap();

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
}

#[cfg(feature = "crossbeam")]
mod crossbeam {
    use super::{calc_size, count};
    use crossbeam_utils::thread;
    use std::collections::HashMap;

    #[allow(dead_code)]
    pub fn frequency(input: &[&str], worker_count: usize) -> HashMap<char, usize> {
        if input.is_empty() || worker_count == 0 {
            return HashMap::new();
        }

        let size = calc_size(input.len(), worker_count);
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
}

#[cfg(feature = "default")]
mod stdthread {
    use super::{calc_size, count};
    use std::collections::HashMap;
    use std::thread;

    #[allow(dead_code)]
    pub fn frequency(input: &[&str], worker_count: usize) -> HashMap<char, usize> {
        if input.is_empty() || worker_count == 0 {
            return HashMap::new();
        }

        let size = calc_size(input.len(), worker_count);
        if size < 5 {
            return count(input);
        }

        let thread_handles = input
            .chunks(size)
            .map(|chunk| {
                let v = chunk
                    .iter()
                    .copied()
                    .map(|s| s.to_string())
                    .collect::<Vec<_>>();

                thread::spawn(move || count(&v))
            })
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
    }
}
