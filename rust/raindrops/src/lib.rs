pub fn raindrops(n: usize) -> String {
    let ppp = vec![3, 5, 7]
        .iter()
        .filter(|i| n % *i == 0)
        .map(|i| match *i {
            3 => "Pling",
            5 => "Plang",
            _ => "Plong",
        })
        .collect::<String>();

    if ppp == "" {
        n.to_string()
    } else {
        ppp
    }
}
