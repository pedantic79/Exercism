#[derive(Debug)]
pub struct HighScores<'a>(&'a [u32]);

impl<'a> HighScores<'a> {
    pub fn new(scores: &'a [u32]) -> Self {
        Self(scores)
    }

    pub fn scores(&self) -> &[u32] {
        self.0
    }

    pub fn latest(&self) -> Option<u32> {
        self.0.iter().last().copied()
    }

    pub fn personal_best(&self) -> Option<u32> {
        self.0.iter().max().copied()
    }

    // Using truncate has the advantage of not reallocating another Vec, as
    // would have been the case if I had done: `v.iter().take(3).collect()`
    // The downside is that truncate does not lower the capacity of the vec,
    // so if the scores is very long, then extra memory is wasted.
    // If this was an issue `Vec::shrink_to_fit` would be useful, but would
    // require another possible allocation, whose performance would depend on
    // the global allocator being used.
    pub fn personal_top_three(&self) -> Vec<u32> {
        let mut v = self.0.to_vec();
        v.sort_unstable_by(|a, b| b.cmp(a));
        v.truncate(3);
        v
    }
}
