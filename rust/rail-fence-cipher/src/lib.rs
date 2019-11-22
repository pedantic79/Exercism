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

        // Place each character on the proper RailFence rail
        for (idx, c) in self.indices().zip(text.chars()) {
            fence[idx].push(c);
        }

        // Combine into a String
        fence.into_iter().collect()
    }

    pub fn decode(&self, cipher: &str) -> String {
        let len = cipher.len();

        let mut fence = self.mk_fence();
        let mut pos = 0;

        // Copies slices of the string into the proper RailFence rail
        // We're copying in reverse order, because we're going to pop everything
        // off to build the output string
        for (rail, frequency) in fence.iter_mut().zip(self.indices_count(len)) {
            rail.extend(cipher[pos..pos + frequency].chars().rev());
            pos += frequency;
        }

        // Use the RailFence to rebuild the String by popping the characters off
        // in reverse order
        self.indices()
            .take(len)
            .fold(String::with_capacity(len), |mut s, idx| {
                s.push(fence[idx].pop().unwrap());
                s
            })
    }

    // Iterator that returns which rail to use, going from 0 to self.0
    // and then back to 0.
    fn indices(&self) -> impl Iterator<Item = usize> {
        let l = self.0.saturating_sub(1);
        (0..=l).chain((1..l).rev()).cycle()
    }

    // Builds a Vec [M; self.0] in size that includes the length of each rail
    fn indices_count(&self, size: usize) -> Vec<usize> {
        self.indices()
            .take(size)
            .fold(vec![0; self.0], |mut freq, idx| {
                freq[idx] += 1;
                freq
            })
    }
}
