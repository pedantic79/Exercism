use std::collections::BTreeMap;


pub fn raindrops(n: usize) -> String {

    let divisible = {
        let mut btm = BTreeMap::new();
        btm.insert(3, "Pling");
        btm.insert(5, "Plang");
        btm.insert(7, "Plong");
        btm
    };

    let v: String = divisible
        .iter()
        .filter(|(&d, _)| n % d == 0)
        .map(|(_, &value)| value)
        .collect();

    if v == "" {
        n.to_string()
    } else {
        v
    }
}
