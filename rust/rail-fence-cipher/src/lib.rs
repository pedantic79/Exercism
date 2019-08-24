pub struct RailFence(usize);

impl RailFence {
    pub fn new(rails: u32) -> Self {
        Self(rails as usize)
    }

    fn mk_fence(&self) -> Vec<String> {
        vec![String::new(); self.0]
    }

    pub fn encode(&self, text: &str) -> String {
        let mut fence = self.mk_fence();

        for (idx, c) in self.indices().zip(text.chars()) {
            fence[idx].push(c);
        }

        fence.into_iter().collect()
    }

    pub fn decode(&self, cipher: &str) -> String {
        let len = cipher.len();
        let indices_freq: Vec<usize> =
            self.indices()
                .take(len)
                .fold(vec![0; self.0], |mut freq, idx| {
                    freq[idx] += 1;
                    freq
                });

        let mut fence = self.mk_fence();
        let mut pos = 0;
        for (rail, frequency) in fence.iter_mut().zip(indices_freq) {
            rail.extend(cipher[pos..pos + frequency].chars().rev());
            pos += frequency;
        }

        self.indices()
            .take(len)
            .fold(String::with_capacity(len), |mut s, idx| {
                s.push(fence[idx].pop().unwrap());
                s
            })
    }

    fn indices(&self) -> impl Iterator<Item = usize> + '_ {
        let l = self.0.saturating_sub(1);
        (0..=l).chain((1..l).rev()).cycle()
    }
}
