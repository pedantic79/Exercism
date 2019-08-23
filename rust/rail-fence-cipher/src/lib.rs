use std::iter::from_fn;

enum Direction {
    Up,
    Down,
}

pub struct RailFence(usize);

impl RailFence {
    pub fn new(rails: u32) -> Self {
        Self(rails as usize)
    }

    fn mk_fence(&self) -> Vec<String> {
        (0..self.0).fold(Vec::with_capacity(self.0), |mut v, _| {
            v.push(String::new());
            v
        })
    }

    pub fn encode(&self, text: &str) -> String {
        let mut fence = self.mk_fence();

        for (idx, c) in self.indices().zip(text.chars()) {
            fence[idx].push(c);
        }

        fence.iter().cloned().collect()
    }

    pub fn decode(&self, cipher: &str) -> String {
        let mut fence = self.mk_fence();
        let mut pos = 0;
        let indices = self.indices().take(cipher.len()).collect::<Vec<_>>();

        for (i, fence_row) in fence.iter_mut().enumerate() {
            let l = indices.iter().filter(|&&n| n == i).count();
            fence_row.extend(cipher[pos..pos + l].chars().rev());
            pos += l;
        }

        self.indices()
            .take(cipher.len())
            .fold(String::new(), |mut s, idx| {
                s.push(fence[idx].pop().unwrap());
                s
            })
    }

    fn indices(&self) -> impl Iterator<Item = usize> + '_ {
        let mut n = 1;
        let mut direction = Direction::Down;

        from_fn(move || {
            n = match direction {
                Direction::Up if n + 1 < self.0 => n + 1,
                Direction::Up => {
                    direction = Direction::Down;
                    n - 1
                }
                Direction::Down if n > 0 => n - 1,
                Direction::Down => {
                    direction = Direction::Up;
                    n + 1
                }
            };

            Some(n)
        })
    }
}
