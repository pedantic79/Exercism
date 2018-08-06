pub fn find() -> Option<u32> {
    let n = 1000;

    for a in 1..n {
        for b in a..n {
            for c in b..n {
                if a * a + b * b == c * c && a + b + c == n {
                    return Some(a * b * c);
                }
            }
        }
    }
    return None;
}
