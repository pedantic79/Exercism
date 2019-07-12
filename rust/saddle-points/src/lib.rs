pub fn find_saddle_points(input: &[Vec<u64>]) -> Vec<(usize, usize)> {
    let h = input.len();
    let w = input[0].len();

    (0..h)
        .flat_map(|x| (0..w).map(|y| (x, y)).collect::<Vec<_>>())
        .filter(|(r, c)| {
            let cmin = input.iter().map(|v| v[*c]).min().unwrap();
            let rmax = input[*r].iter().cloned().max().unwrap();

            let cell = input[*r][*c];
            cell >= rmax && cell <= cmin
        })
        .collect()
}
