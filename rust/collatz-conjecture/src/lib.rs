pub fn collatz(n: u64) -> Option<u64> {
    if n == 0 {
        None
    } else {
        Some(find_collatz(n, 0))
    }
}

fn find_collatz(n: u64, count: u64) -> u64 {
    if n == 1 {
        count
    } else if n % 2 == 0 {
        find_collatz(n / 2, count + 1)
    } else {
        find_collatz(n * 3 + 1, count + 1)
    }
}
