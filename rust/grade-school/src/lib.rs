use std::collections::HashMap;

#[derive(Default)]
pub struct School {
    roster: HashMap<u32, Vec<String>>,
}

impl School {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add(&mut self, grade: u32, student: &str) {
        let entry = self.roster.entry(grade).or_insert_with(Vec::new);
        entry.push(student.to_string());
        entry.sort()
    }

    pub fn grades(&self) -> Vec<u32> {
        let mut grades = self.roster.keys().cloned().collect::<Vec<_>>();
        grades.sort();
        grades
    }

    // If grade returned an `Option<&Vec<String>>`,
    // the internal implementation would be forced to keep a `Vec<String>` to lend out.
    // By returning an owned vector instead,
    // the internal implementation is free to use whatever it chooses.
    pub fn grade(&self, grade: u32) -> Option<Vec<String>> {
        self.roster.get(&grade).cloned()
    }
}
