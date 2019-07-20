use std::iter::repeat;

pub fn encrypt(input: &str) -> String {
    let v = input
        .chars()
        .filter(char::is_ascii_alphanumeric)
        .map(|c| c.to_ascii_lowercase())
        .collect::<Vec<_>>();

    if v.is_empty() {
        String::new()
    } else {
        let (width, height) = get_dimensions(v.len());
        let mut matrix = vec![String::with_capacity(height); width];

        for chunk in v.chunks(width) {
            matrix
                .iter_mut()
                .zip(chunk.iter().chain(repeat(&' ')))
                .for_each(|(row, &c)| row.push(c));
        }

        matrix.join(" ")
    }
}

fn get_dimensions(l: usize) -> (usize, usize) {
    let sqrt = (l as f64).sqrt();

    (sqrt.ceil() as usize, sqrt as usize)
}
