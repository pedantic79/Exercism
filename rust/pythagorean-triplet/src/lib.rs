use rayon::iter::{IntoParallelIterator, ParallelIterator};
use std::collections::HashSet;

pub fn find(sum: u32) -> HashSet<[u32; 3]> {
    (1..(sum / 3))
        .into_par_iter()
        .map(|a| {
            let b_c = sum - a;
            let b = (b_c * b_c - a * a) / (2 * b_c);
            [a, b, b_c - b]
        })
        .filter(|[a, b, c]| a < b && a * a + b * b == c * c)
        .collect()
}
