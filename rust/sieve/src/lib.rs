use std::collections::BTreeMap;

pub fn primes_up_to(upper_bound: u64) -> Vec<u64> {
    let mut sieve: BTreeMap<u64, bool> = (2..=upper_bound).map(|i| (i, true)).collect();

    for i in 2..=upper_bound {
        if let Some(true) = sieve.get(&i) {
            for j in (2 * i..=upper_bound).step_by(i as usize) {
                sieve.insert(j, false);
            }
        }
    }

    sieve.iter().filter(|(_, v)| **v).map(|(k, _)| *k).collect()
}
