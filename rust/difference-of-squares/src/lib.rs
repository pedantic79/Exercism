pub fn square_of_sum(n: u32) -> u32 {
    (n.pow(4) + 2 * n.pow(3) + n.pow(2)) / 4
}

pub fn sum_of_squares(n: u32) -> u32 {
    (n + 3 * n.pow(2) + 2 * n.pow(3)) / 6
}

pub fn difference(n: u32) -> u32 {
    square_of_sum(n) - sum_of_squares(n)
}
