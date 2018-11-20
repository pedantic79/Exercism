#[macro_use]
extern crate indexmap;

pub fn raindrops(n: usize) -> String {
    let divisible = indexmap!{
        3 => "Pling",
        5 => "Plang",
        7 => "Plong",
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
