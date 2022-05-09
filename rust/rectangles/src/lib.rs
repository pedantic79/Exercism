pub fn count(input: &[&str]) -> usize {
    let rows = input.len();
    let cols = input.get(0).map(|line| line.len()).unwrap_or(0);

    input
        .iter()
        .enumerate()
        .flat_map(|(r, line)| {
            line.bytes()
                .enumerate()
                .filter_map(move |(c, cell)| if cell == b'+' { Some((r, c)) } else { None })
        })
        .flat_map(|(row, col)| {
            let x = cols - col;
            let y = rows - row;

            (1..x).flat_map(move |col_offset| {
                (1..y).filter(move |&row_offset| {
                    let corners = [
                        (row, col),
                        (row, col + col_offset),
                        (row + row_offset, col),
                        (row + row_offset, col + col_offset),
                    ];
                    has_corners(input, corners) && has_edges(input, corners)
                })
            })
        })
        .count()
}

fn has_corners(input: &[&str], corners: [(usize, usize); 4]) -> bool {
    corners
        .iter()
        .all(|&(r, c)| input[r].as_bytes().get(c) == Some(&b'+'))
}

fn has_edges(input: &[&str], corners: [(usize, usize); 4]) -> bool {
    let hori = [(corners[0], corners[1]), (corners[2], corners[3])];
    let vert = [(corners[0], corners[2]), (corners[1], corners[3])];

    hori.iter()
        .all(|&((row, col_left), (_, col_right))| {
            input[row]
                .bytes()
                .skip(col_left)
                .take((col_right - col_left) + 1)
                .all(|c| c == b'+' || c == b'-')
        })
        .then(|| {
            vert.iter().all(|&((row_top, col), (row_bottom, _))| {
                input
                    .iter()
                    .filter_map(move |line| line.as_bytes().get(col))
                    .skip(row_top)
                    .take((row_bottom - row_top) + 1)
                    .all(|&c| c == b'+' || c == b'|')
            })
        })
        .unwrap_or(false)
}
