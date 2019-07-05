pub fn raindrops(n: usize) -> String {
    let divisible = [(3, "Pling"), (5, "Plang"), (7, "Plong")];

    let v: String = divisible
        .iter()
        .filter(|&(d, _)| n % d == 0)
        .map(|&(_, value)| value)
        .collect();

    if v.is_empty() {
        n.to_string()
    } else {
        v
    }
}
