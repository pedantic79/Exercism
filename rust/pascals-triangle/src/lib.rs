use std::iter::once;

pub struct PascalsTriangle {
    rows: Vec<Vec<u32>>,
}

impl PascalsTriangle {
    pub fn new(row_count: u32) -> Self {
        let mut rows: Vec<Vec<u32>> = Vec::with_capacity(row_count as usize);
        for _ in 0..row_count {
            let row = match rows.iter().last() {
                Some(last) => Self::next_row(last),
                None => vec![1],
            };
            rows.push(row);
        }

        Self { rows }
    }

    pub fn rows(&self) -> Vec<Vec<u32>> {
        self.rows.clone()
    }

    fn next_row(old_row: &[u32]) -> Vec<u32> {
        once(1)
            .chain(old_row.windows(2).map(|x| x[0] + x[1]))
            .chain(once(1))
            .collect()
    }
}
