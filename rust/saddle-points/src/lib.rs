pub fn find_saddle_points(input: &[Vec<u64>]) -> Vec<(usize, usize)> {
    let h = input.len();
    let w = input[0].len();

    (0..h)
        .flat_map(|y| (0..w).map(move |x| (y, x)))
        .filter(|&(r, c)| {
            let cmin = input.iter().map(|v| v[c]).min();
            let rmax = input[r].iter().max();
            let cell = input[r][c];

            rmax.and_then(|&rmax| cmin.map(|cmin| cell >= rmax && cell <= cmin))
                .unwrap_or(false)
        })
        .collect()
}
