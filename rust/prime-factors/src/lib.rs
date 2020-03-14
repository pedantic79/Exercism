pub fn factors(n: u64) -> Vec<u64> {
    let mut v = Vec::new();
    let mut n = n;
    let mut divisor = 2;

    while n > 1 {
        if n % divisor == 0 {
            v.push(divisor);
            n /= divisor
        } else {
            divisor  += 1
        }
    }

    v
}
