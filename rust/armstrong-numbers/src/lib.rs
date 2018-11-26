pub fn is_armstrong_number(num: u32) -> bool {
    let num_digits = digits(num);
    let p = num_digits.len() as u32;
    let s: u32 = num_digits.iter().map(|d| d.pow(p)).sum();

    s == num
}

fn digits(n: u32) -> Vec<u32> {
    n.to_string()
        .chars()
        .map(|c| c.to_digit(10).unwrap())
        .collect()
}
