pub fn nth(n: u32) -> u32 {
    let mut primes = vec![2, 3];

    for k in 0.. {
        if primes.len() > n as usize {
            break;
        }

        // Use Pritchard's Prime Wheel Sieve with 2, 3 spokes
        // This produces prime candidates in the form 6k + 1 and 6k + 5
        // This is equivalent of skipping all multiples of 2 and 3. Besides,
        // 1; 5 is the only number less than 6 that is coprime to 2 and 3.
        //
        // Using more spokes would be more efficient, but would require to
        // hardcode in more coprime values. For example, a 2,3,5 spoke wheel
        // would have candidates of 30k+{1,7,11,13,17,19,23,29}
        //
        // A basic odd only Sieve of Eratosthenes (i.e. 2 and every odd number
        // larger than 3) is equivalent to a Prime Wheel with just a 2 spoke.
        // It's candidates are 2k+1
        //
        // Also, we need to remove k=0, r=1, which is the case where the
        // candidate is 1, which we don't consider a prime
        for &r in &[1, 5] {
            let cand = 6 * k + r;

            if cand > 1
                && !primes
                    .iter()
                    .take_while(|&x| x * x <= cand)
                    .any(|&p| cand % p == 0)
            {
                primes.push(cand);
            }
        }
    }

    primes[n as usize]
}
