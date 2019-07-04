pub fn primes_up_to(upper_bound: u64) -> Vec<u64> {
    let upper_bound = upper_bound as usize;
    let mut sieve: Vec<bool> = vec![true; upper_bound + 1];

    for i in 2..=upper_bound {
        if sieve[i] {
            for j in (i..=upper_bound).step_by(i).skip(1) {
                sieve[j] = false;
            }
        }
    }

    sieve
        .iter()
        .enumerate()
        .skip(2)
        .filter(|x| *x.1)
        .map(|x| x.0 as u64)
        .collect()
}

pub fn primes_up_to_btm(upper_bound: u64) -> Vec<u64> {
    use std::collections::BTreeMap;

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

pub fn primes_up_to_hm(upper_bound: u64) -> Vec<u64> {
    use std::collections::HashMap;

    let mut sieve: HashMap<u64, bool> = (2..=upper_bound).map(|i| (i, true)).collect();

    for i in 2..=upper_bound {
        if let Some(true) = sieve.get(&i) {
            for j in (2 * i..=upper_bound).step_by(i as usize) {
                sieve.insert(j, false);
            }
        }
    }

    sieve.iter().filter(|(_, v)| **v).map(|(k, _)| *k).collect()
}
