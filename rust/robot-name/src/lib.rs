use lazy_static::lazy_static;
use rand::Rng;
use std::collections::HashSet;
use std::sync::Mutex;

lazy_static! {
    static ref ROBOT_NAMES: Mutex<HashSet<String>> = Mutex::new(HashSet::new());
}

#[derive(PartialOrd, Ord, PartialEq, Eq)]
pub struct Robot {
    name: String,
}

impl Robot {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn name(&self) -> &str {
        self.name.as_ref()
    }

    pub fn reset_name(&mut self) {
        ROBOT_NAMES.lock().unwrap().remove(&self.name);
        self.name = Self::gen_unique_name();
    }

    fn gen_name() -> String {
        let mut rng = rand::thread_rng();

        let n: u32 = rng.gen_range(0, 1000);
        let a1: char = rng.gen_range(b'A', b'Z') as char;
        let a2: char = rng.gen_range(b'A', b'Z') as char;
        format!("{}{}{:03}", a1, a2, n)
    }

    fn gen_unique_name() -> String {
        loop {
            let name = Self::gen_name();
            let mut hs = ROBOT_NAMES.lock().unwrap();
            if !hs.contains(&name) {
                hs.insert(name.clone());
                return name;
            }
        }
    }
}

impl Default for Robot {
    fn default() -> Self {
        Self {
            name: Self::gen_unique_name(),
        }
    }
}

impl Drop for Robot {
    fn drop(&mut self) {
        ROBOT_NAMES.lock().unwrap().remove(&self.name);
    }
}

#[cfg(test)]
mod tests {
    use super::Robot;
    use std::collections::BTreeSet;

    #[test]
    fn unique_name() {
        let mut robots: BTreeSet<Robot> = BTreeSet::new();

        // Generate 10000 robots, make sure that they are all uniquely named
        for _ in 0..10000 {
            let r = Robot::new();

            assert!(!robots.contains(&r));
            robots.insert(r);
        }
    }
}
