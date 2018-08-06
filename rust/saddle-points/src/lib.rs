pub fn find_saddle_points(input: &[Vec<u64>]) -> Vec<(usize, usize)> {
    let mut points = Vec::new();

    for (r, row) in input.iter().enumerate() {
        for c in 0..row.len() {
            let cell = input[r][c];
            let col: Vec<u64> = input.iter().map(|v| v[c]).collect();

            match row.iter().max() {
                Some(rmax) => match col.iter().min() {
                    Some(cmin) => {
                        if cell >= *rmax && cell <= *cmin {
                            points.push((r, c));
                        }
                    },
                    None => {},
                },
                None => {},
            }
        }
    }
    points
}
