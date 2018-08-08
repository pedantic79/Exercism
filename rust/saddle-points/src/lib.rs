pub fn find_saddle_points(input: &[Vec<u64>]) -> Vec<(usize, usize)> {
    let h = input.len();
    let w = input[0].len();

    (0..h)
        .flat_map(|x| (0..w).map(|y| (x, y)).collect::<Vec<(usize, usize)>>())
        .filter(|(r, c)| {
            let cell = input[*r][*c];
            let col: Vec<u64> = input.iter().map(|v| v[*c]).collect();

            let rmax = input[*r].iter().max().unwrap();
            let cmin = col.iter().min().unwrap();
            cell >= *rmax && cell <= *cmin
        })
        .collect::<Vec<(usize, usize)>>()
}
