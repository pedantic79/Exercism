use std::collections::BTreeMap;
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
        let indices_map: BTreeMap<usize, usize> =
            self.indices()
                .take(cipher.len())
                .fold(BTreeMap::new(), |mut btm, idx| {
                    *btm.entry(idx).or_insert(0) += 1;
                    btm
                });

        let mut fence = self.mk_fence();
        let mut pos = 0;
        for (rail, frequency) in fence.iter_mut().zip(indices_map.values()) {
            rail.extend(cipher[pos..pos + frequency].chars().rev());
            pos += frequency;
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
                Direction::Up if n + 1 == self.0 => {
                    direction = Direction::Down;
                    n - 1
                }
                Direction::Up => n + 1,

                Direction::Down if n == 0 => {
                    direction = Direction::Up;
                    n + 1
                }
                Direction::Down => n - 1,
            };

            Some(n)
        })
    }
}
