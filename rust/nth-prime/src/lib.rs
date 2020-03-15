pub fn nth(n: u32) -> u32 {
    let n = n as usize;
    let spoke = [2, 3];
    let mut primes = vec![5];

    for k in 1.. {
        if primes.len() + spoke.len() > n {
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
        // We're skipping k=0, and seeding the value of k=0, r=5 into primes
        //
        for &r in &[1, 5] {
            let cand = 6 * k + r;

            if !primes
                .iter()
                .take_while(|&x| x * x <= cand)
                .any(|&p| cand % p == 0)
            {
                primes.push(cand);
            }
        }
    }

    unsafe {
        append(&mut primes, &spoke);
    }

    // if n < spoke.len() {
    //     spoke[n]
    // } else {
    //     primes[n - spoke.len()]
    // }
    primes[n]
}

// This appends `slice` to `v`, this works the same way that `String::push_str`
unsafe fn append<T>(v: &mut Vec<T>, slice: &[T]) {
    let len = v.len();
    let amt = slice.len();
    v.reserve(amt);

    std::ptr::copy(v.as_ptr(), v.as_mut_ptr().add(amt), len);
    std::ptr::copy(slice.as_ptr(), v.as_mut_ptr(), amt);
    v.set_len(amt + len);
}
